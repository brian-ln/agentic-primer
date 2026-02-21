import type { ScreenshotArtifact } from "../screen-capture/interface.js";

export interface UploadResult {
  id: string;
  r2Key: string;
  capturedAt: string;
  shareUrl: string;
}

export interface ScreenStorageActor {
  store(artifact: ScreenshotArtifact, note: string): Promise<UploadResult>;
}

/**
 * @deprecated Use ScreenStorageActor / UploadResult instead.
 * Kept for backward-compat until all callers are migrated.
 */
export interface StoredArtifact {
  id: string;
  shareUrl: string;
  r2Key: string;
  storedAt: string;
}

/**
 * @deprecated Use ScreenStorageActor instead.
 */
export interface AssetStorageActor {
  store(artifact: ScreenshotArtifact, bytes: Uint8Array): Promise<StoredArtifact>;
}
