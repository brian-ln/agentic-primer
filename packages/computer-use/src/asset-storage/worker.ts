/// <reference types="bun-types" />
import type { ScreenshotArtifact } from "../screen-capture/interface.js";
import type { ScreenStorageActor, UploadResult } from "./interface.js";
import { ensureAuth, presignedUpload } from "./upload.js";

export interface WorkerAssetStorageConfig {
  /** OAuth2 client ID for device flow (default: 'shot-cli') */
  clientId?: string;
  /** API base URL (default: 'https://brianln.ai') */
  apiUrl?: string;
}

/**
 * WorkerAssetStorageActor — client-side (Bun/Node) implementation.
 *
 * Uses RFC 8628 device flow for auth, storing tokens in
 * ~/.claude/config/brianln-auth.json keyed by clientId.
 *
 * Upload is a 3-step presigned URL flow:
 *   1. POST /api/assets/screenshot/init   → { artifactId, uploadUrl, r2Key }
 *   2. PUT <uploadUrl>                    → (raw PNG bytes, direct to R2)
 *   3. POST /api/assets/screenshot/complete { artifactId } → { id, r2Key, shareUrl, capturedAt }
 *
 * DO NOT import this class in Cloudflare Workers — use CloudflareAssetStorageActor instead.
 */
export class WorkerAssetStorageActor implements ScreenStorageActor {
  private readonly clientId: string;
  private readonly apiUrl: string;

  constructor(config: WorkerAssetStorageConfig = {}) {
    this.clientId = config.clientId ?? 'shot-cli';
    this.apiUrl = config.apiUrl ?? 'https://brianln.ai';
  }

  async store(artifact: ScreenshotArtifact, note: string): Promise<UploadResult> {
    // Obtain a valid bearer token (device flow if needed)
    const { token, apiUrl } = await ensureAuth(this.clientId, this.apiUrl);

    // Read the PNG bytes from localPath (set by NativeScreenCaptureActor)
    let pngBytes: Uint8Array;
    if (artifact.data) {
      pngBytes = artifact.data;
    } else if (artifact.localPath) {
      const ab = await Bun.file(artifact.localPath).arrayBuffer();
      pngBytes = new Uint8Array(ab);
    } else {
      throw new Error('WorkerAssetStorageActor: artifact has neither .data nor .localPath');
    }

    process.stderr.write('Uploading...\n');

    const result = await presignedUpload(apiUrl, token, pngBytes, {
      note: note || artifact.note || '',
      sourceActor: artifact.sourceActor,
    });

    return {
      id: result.id,
      r2Key: result.r2Key,
      capturedAt: result.capturedAt,
      shareUrl: result.shareUrl,
    };
  }
}
