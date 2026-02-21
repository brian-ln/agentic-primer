/// <reference types="bun-types" />
/**
 * upload.ts — INTERNAL MODULE. Not exported from package.
 *
 * Contains:
 *   - AuthStore: read/write ~/.claude/config/brianln-auth.json (keyed by clientId)
 *   - runDeviceFlow: RFC 8628 device authorization flow
 *   - ensureAuth: validates token, triggers device flow if needed
 *   - presignedUpload: 3-step upload (init → PUT → complete)
 */

// ── Auth store ────────────────────────────────────────────────────────────────

const AUTH_FILE = `${process.env.HOME}/.claude/config/brianln-auth.json`;
const LEGACY_SHOT_FILE = `${process.env.HOME}/.claude/config/shot.json`;

export interface ClientAuthEntry {
  token?: string;
  tokenExpiry?: number;
  apiUrl: string;
}

type AuthStore = Record<string, ClientAuthEntry>;

async function readAuthStore(): Promise<AuthStore> {
  // Try new unified location first
  const file = Bun.file(AUTH_FILE);
  if (await file.exists()) {
    try {
      return JSON.parse(await file.text()) as AuthStore;
    } catch {
      return {};
    }
  }
  // Migrate from legacy shot.json if it exists
  const legacy = Bun.file(LEGACY_SHOT_FILE);
  if (await legacy.exists()) {
    try {
      const data = JSON.parse(await legacy.text()) as ClientAuthEntry & { apiToken?: string };
      const store: AuthStore = {
        'shot-cli': {
          token: data.token,
          tokenExpiry: data.tokenExpiry,
          apiUrl: data.apiUrl ?? 'https://brianln.ai',
        },
      };
      // Write migrated data to new location
      await writeAuthStore(store);
      process.stderr.write(`[auth] Migrated ~/.claude/config/shot.json → ${AUTH_FILE}\n`);
      return store;
    } catch {
      return {};
    }
  }
  return {};
}

async function writeAuthStore(store: AuthStore): Promise<void> {
  // Ensure directory exists
  const dir = AUTH_FILE.substring(0, AUTH_FILE.lastIndexOf('/'));
  const { mkdirSync } = await import('node:fs');
  try { mkdirSync(dir, { recursive: true }); } catch { /* already exists */ }
  await Bun.write(AUTH_FILE, JSON.stringify(store, null, 2) + '\n');
}

// ── Device flow (RFC 8628) ────────────────────────────────────────────────────

export async function runDeviceFlow(
  apiUrl: string,
  clientId: string,
): Promise<{ token: string; tokenExpiry: number }> {
  // Step 1: Request device + user codes
  const initRes = await fetch(`${apiUrl}/device`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
    body: `client_id=${encodeURIComponent(clientId)}&scope=write`,
  });
  if (!initRes.ok) {
    throw new Error(`Device flow init failed: ${initRes.status}`);
  }
  const init = await initRes.json() as {
    device_code: string;
    user_code: string;
    verification_uri_complete: string;
    expires_in: number;
    interval: number;
  };

  // Step 2: Prompt user
  process.stderr.write(`\n  Authorize ${clientId}:\n`);
  process.stderr.write(`  ${init.verification_uri_complete}\n`);
  process.stderr.write(`  Code: ${init.user_code}\n`);
  process.stderr.write(`  Waiting for approval...\n\n`);

  // Step 3: Poll
  let interval = (init.interval || 5) * 1000;
  const deadline = Date.now() + init.expires_in * 1000;

  while (Date.now() < deadline) {
    await new Promise(r => setTimeout(r, interval));
    const pollRes = await fetch(`${apiUrl}/device/token`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
      body: `grant_type=urn%3Aietf%3Aparams%3Aoauth%3Agrant-type%3Adevice_code&device_code=${encodeURIComponent(init.device_code)}&client_id=${encodeURIComponent(clientId)}`,
    });
    const result = await pollRes.json() as any;
    if (result.access_token) {
      return {
        token: result.access_token,
        tokenExpiry: Math.floor(Date.now() / 1000) + (result.expires_in || 3600) - 60,
      };
    }
    if (result.error === 'slow_down') {
      interval = (result.interval || 10) * 1000;
    } else if (result.error === 'authorization_pending') {
      // keep polling
    } else {
      throw new Error(`Device flow error: ${result.error} — ${result.error_description || ''}`);
    }
  }
  throw new Error('Device flow timed out');
}

// ── ensureAuth ────────────────────────────────────────────────────────────────

export async function ensureAuth(
  clientId: string,
  apiUrl: string,
): Promise<{ token: string; apiUrl: string }> {
  const store = await readAuthStore();
  const entry = store[clientId] ?? { apiUrl };
  const now = Math.floor(Date.now() / 1000);

  // Valid JWT token
  if (entry.token && entry.tokenExpiry && now < entry.tokenExpiry) {
    return { token: entry.token, apiUrl: entry.apiUrl ?? apiUrl };
  }

  // Run device flow to get a new token
  process.stderr.write(`[auth] No valid token for ${clientId} — starting device authorization flow.\n`);
  const resolvedApiUrl = entry.apiUrl ?? apiUrl;
  const { token, tokenExpiry } = await runDeviceFlow(resolvedApiUrl, clientId);

  // Persist
  const newEntry: ClientAuthEntry = { token, tokenExpiry, apiUrl: resolvedApiUrl };
  store[clientId] = newEntry;
  await writeAuthStore(store);
  process.stderr.write(`[auth] Authorized. Token saved to ${AUTH_FILE}\n\n`);

  return { token, apiUrl: resolvedApiUrl };
}

// ── 3-step presigned URL upload ───────────────────────────────────────────────

export interface InitPayload {
  note: string;
  sourceActor: string;
  sessionId?: string;
  taskId?: string;
}

export interface InitResponse {
  artifactId: string;
  uploadUrl: string;
  r2Key: string;
}

export interface CompleteResponse {
  id: string;
  r2Key: string;
  shareUrl: string;
  capturedAt: string;
  note: string;
  sourceActor: string;
  bytes: number;
}

export async function presignedUpload(
  apiUrl: string,
  token: string,
  pngBytes: Uint8Array,
  init: InitPayload,
): Promise<CompleteResponse> {
  const authHeader = { 'Authorization': `Bearer ${token}` };

  // Step 1: Init — allocate artifact ID + get presigned PUT URL
  const initRes = await fetch(`${apiUrl}/api/assets/upload/init`, {
    method: 'POST',
    headers: { ...authHeader, 'Content-Type': 'application/json' },
    body: JSON.stringify({
      note: init.note,
      sourceActor: init.sourceActor,
      ...(init.sessionId ? { sessionId: init.sessionId } : {}),
      ...(init.taskId ? { taskId: init.taskId } : {}),
    }),
  });

  if (!initRes.ok) {
    throw new Error(`Upload init failed: ${initRes.status} — ${await initRes.text().catch(() => '')}`);
  }

  const { artifactId, uploadUrl, r2Key } = await initRes.json() as InitResponse;

  // Step 2: PUT raw PNG bytes directly to R2 via presigned URL (no auth header — presigned)
  // Cast to ArrayBuffer: bun-types BodyInit requires ArrayBuffer, not Uint8Array<ArrayBufferLike>
  const putRes = await fetch(uploadUrl, {
    method: 'PUT',
    headers: { 'Content-Type': 'image/png' },
    body: pngBytes.buffer as ArrayBuffer,
  });

  if (!putRes.ok) {
    throw new Error(`Presigned PUT failed: ${putRes.status} — ${await putRes.text().catch(() => '')}`);
  }

  // Step 3: Complete — validate upload, generate share token, emit signal
  const completeRes = await fetch(`${apiUrl}/api/assets/upload/complete`, {
    method: 'POST',
    headers: { ...authHeader, 'Content-Type': 'application/json' },
    body: JSON.stringify({ artifactId }),
  });

  if (!completeRes.ok) {
    throw new Error(`Upload complete failed: ${completeRes.status} — ${await completeRes.text().catch(() => '')}`);
  }

  return completeRes.json() as Promise<CompleteResponse>;
}
