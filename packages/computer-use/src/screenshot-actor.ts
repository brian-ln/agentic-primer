/// <reference types="bun-types" />
/**
 * ScreenshotActor — duck-typed UGS actor (does NOT import from ugs/ to avoid circular dep).
 *
 * Receives messages with params:
 *   { mode?, note?, upload?, clientId?, attachTo? }
 *
 * On receive:
 *   1. Captures a screenshot via NativeScreenCaptureActor
 *   2. Optionally uploads via WorkerAssetStorageActor
 *   3. Optionally calls attachFn with the artifact (e.g., to link it to a graph node)
 */

import type { CaptureParams, ScreenshotArtifact } from "./screen-capture/interface.js";
import { NativeScreenCaptureActor } from "./screen-capture/native/index.js";
import { WorkerAssetStorageActor } from "./asset-storage/worker.js";

/**
 * Optional callback to attach a captured artifact to an external graph node.
 * Duck-typed — does not import UGS types.
 */
export type NodeAttachFn = (
  artifact: ScreenshotArtifact,
  attachTo: string,
) => Promise<void> | void;

export interface ScreenshotActorConfig {
  attachFn?: NodeAttachFn;
}

export interface ScreenshotMessage {
  /** Capture mode (default: 'interactive') */
  mode?: CaptureParams["mode"];
  /** Note/label for the screenshot */
  note?: string;
  /** Whether to upload to the Worker (default: false) */
  upload?: boolean;
  /** OAuth client ID for device flow (default: 'shot-cli') */
  clientId?: string;
  /** Optional graph node ID to attach the artifact to */
  attachTo?: string;
}

export class ScreenshotActor {
  private readonly attachFn?: NodeAttachFn;

  constructor(config: ScreenshotActorConfig = {}) {
    this.attachFn = config.attachFn;
  }

  /**
   * Duck-typed UGS actor receive method.
   * Compatible with router.registerActor() — does NOT inherit from Actor base class.
   */
  async receive(message: { payload?: ScreenshotMessage; [key: string]: any }): Promise<{
    success: boolean;
    payload?: ScreenshotArtifact;
    error?: string;
  }> {
    const params: ScreenshotMessage = message.payload ?? {};

    const captureActor = new NativeScreenCaptureActor();

    let artifact: ScreenshotArtifact;
    try {
      artifact = await captureActor.capture({
        mode: params.mode ?? 'interactive',
        note: params.note,
      });
    } catch (e) {
      const msg = e instanceof Error ? e.message : String(e);
      return { success: false, error: `Capture failed: ${msg}` };
    }

    // Optionally upload
    if (params.upload && artifact.localPath) {
      try {
        const storageActor = new WorkerAssetStorageActor({
          clientId: params.clientId ?? 'shot-cli',
        });
        const result = await storageActor.store(artifact, params.note ?? artifact.note ?? '');
        artifact = { ...artifact, r2Key: result.r2Key, shareUrl: result.shareUrl };
      } catch (e) {
        const msg = e instanceof Error ? e.message : String(e);
        process.stderr.write(`[ScreenshotActor] Upload failed (non-fatal): ${msg}\n`);
      }
    }

    // Optionally attach to a graph node
    if (params.attachTo && this.attachFn) {
      try {
        await this.attachFn(artifact, params.attachTo);
      } catch (e) {
        const msg = e instanceof Error ? e.message : String(e);
        process.stderr.write(`[ScreenshotActor] attachFn failed (non-fatal): ${msg}\n`);
      }
    }

    return { success: true, payload: artifact };
  }
}
