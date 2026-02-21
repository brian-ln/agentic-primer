import type { ScreenshotArtifact } from "../screen-capture/interface.js";
import type { ScreenStorageActor, UploadResult } from "./interface.js";

// Minimal Cloudflare Worker binding types (full types: @cloudflare/workers-types)
// Declared here so this file type-checks with bun-types tsconfig.
interface R2Bucket {
  put(
    key: string,
    value: ArrayBuffer | ArrayBufferView | ReadableStream | string | null,
    options?: { httpMetadata?: { contentType?: string }; customMetadata?: Record<string, string> },
  ): Promise<unknown>;
}
interface D1Database {
  prepare(sql: string): D1PreparedStatement;
}
interface D1PreparedStatement {
  bind(...values: unknown[]): D1PreparedStatement;
  run(): Promise<unknown>;
}

export interface CloudflareAssetStorageConfig {
  /** R2 bucket binding (from Cloudflare Worker environment) */
  bucket: R2Bucket;
  /** D1 database binding (from Cloudflare Worker environment) */
  db: D1Database;
  /** Cloudflare account ID (for public R2 URLs) */
  accountId?: string;
  /** R2 bucket name (default: 'knowledge') */
  bucketName?: string;
  /** Base URL for share links (default: 'https://brianln.ai') */
  baseUrl?: string;
}

/**
 * CloudflareAssetStorageActor — server-side (Cloudflare Worker) implementation.
 *
 * Uses R2 binding for direct upload (no presigned URL, no device flow).
 * Intended for use inside Cloudflare Workers where R2/D1 bindings are available.
 *
 * DO NOT use WorkerAssetStorageActor in Workers — it uses Bun filesystem APIs
 * and device flow which are unavailable in the CF runtime.
 */
export class CloudflareAssetStorageActor implements ScreenStorageActor {
  private readonly bucket: R2Bucket;
  private readonly db: D1Database;
  private readonly accountId: string;
  private readonly bucketName: string;
  private readonly baseUrl: string;

  constructor(config: CloudflareAssetStorageConfig) {
    this.bucket = config.bucket;
    this.db = config.db;
    this.accountId = config.accountId ?? '';
    this.bucketName = config.bucketName ?? 'knowledge';
    this.baseUrl = config.baseUrl ?? 'https://brianln.ai';
  }

  async store(artifact: ScreenshotArtifact, note: string): Promise<UploadResult> {
    const pngBytes = artifact.data;
    if (!pngBytes) {
      throw new Error('CloudflareAssetStorageActor: artifact.data is required (no filesystem access in Workers)');
    }

    const now = new Date();
    const capturedAt = artifact.capturedAt ?? now.toISOString();
    const yyyy = now.getUTCFullYear().toString();
    const mm = String(now.getUTCMonth() + 1).padStart(2, '0');
    const dd = String(now.getUTCDate()).padStart(2, '0');
    const ts = `${yyyy}${mm}${dd}_${String(now.getUTCHours()).padStart(2, '0')}${String(now.getUTCMinutes()).padStart(2, '0')}${String(now.getUTCSeconds()).padStart(2, '0')}`;
    const safeNote = (note || artifact.note || '').replace(/[^a-zA-Z0-9_-]/g, '_').slice(0, 64);
    const filename = safeNote ? `screenshot_${ts}_${safeNote}.png` : `screenshot_${ts}.png`;
    const r2Key = `screenshots/${yyyy}/${mm}/${dd}/${filename}`;
    const id = artifact.id ?? `shot-${ts}`;

    // Direct R2 write via binding
    await this.bucket.put(r2Key, pngBytes, {
      httpMetadata: { contentType: 'image/png' },
      customMetadata: {
        note: safeNote,
        source_actor: artifact.sourceActor,
        captured_at: capturedAt,
      },
    });

    // Persist to D1
    await this.db.prepare(`
      INSERT INTO screenshot_artifacts (
        id, r2_key, r2_bucket, r2_account,
        captured_at, note, source_actor, bytes, lifecycle
      ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, 'uploaded')
    `).bind(
      id,
      r2Key,
      this.bucketName,
      this.accountId,
      capturedAt,
      safeNote,
      artifact.sourceActor,
      pngBytes.byteLength,
    ).run();

    // Generate share token
    const shareToken = Array.from(crypto.getRandomValues(new Uint8Array(32)))
      .map(b => b.toString(16).padStart(2, '0')).join('');
    await this.db.prepare(
      `UPDATE screenshot_artifacts SET share_token = ? WHERE id = ?`
    ).bind(shareToken, id).run();

    const shareUrl = `${this.baseUrl}/s/${shareToken}`;

    return { id, r2Key, capturedAt, shareUrl };
  }
}
