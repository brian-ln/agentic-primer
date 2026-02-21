/// <reference lib="dom" />
import type { ScreenCaptureActor, CaptureParams, ScreenshotArtifact } from "./interface.ts";

/**
 * BrowserScreenCaptureActor — captures screenshots in browser environments.
 *
 * Uses navigator.mediaDevices.getDisplayMedia() for screen capture,
 * then draws to canvas and exports as PNG blob.
 *
 * Requires user gesture (screen share permission prompt will appear).
 *
 * NOTE: Upload to R2 from browser context requires a Worker endpoint.
 * Call POST /api/assets/screenshot on a Worker with the blob.
 * r2Key and presignedUrl are populated by the Worker response.
 */
export class BrowserScreenCaptureActor implements ScreenCaptureActor {
  readonly platform = "browser";

  async capture(params: CaptureParams): Promise<ScreenshotArtifact> {
    // getDisplayMedia requires browser context + user gesture
    if (typeof navigator === "undefined" || !navigator.mediaDevices?.getDisplayMedia) {
      throw new Error(
        "BrowserScreenCaptureActor requires a browser environment with getDisplayMedia support"
      );
    }

    const id = `shot-${Date.now()}`;
    const capturedAt = new Date().toISOString();

    // Request screen capture
    const stream = await navigator.mediaDevices.getDisplayMedia({
      video: { frameRate: 1 },
      audio: false,
    });

    // Capture a single frame via ImageCapture API or canvas
    const track = stream.getVideoTracks()[0];
    // grabFrame() exists in the spec but is missing from some TS DOM lib versions
    const imageCapture = new ImageCapture(track);
    const bitmap = await (imageCapture as unknown as { grabFrame(): Promise<ImageBitmap> }).grabFrame();
    track.stop();

    // Draw to canvas
    const canvas = document.createElement("canvas");
    canvas.width = bitmap.width;
    canvas.height = bitmap.height;
    const ctx = canvas.getContext("2d")!;
    ctx.drawImage(bitmap, 0, 0);

    // Export as PNG blob
    const blob = await new Promise<Blob>((resolve, reject) =>
      canvas.toBlob((b) => (b ? resolve(b) : reject(new Error("toBlob failed"))), "image/png")
    );

    const bytes = blob.size;

    return {
      id,
      r2Bucket: "knowledge",
      r2Account: "8d78f1135e2ebd70b5c8f5dee9d519ff",
      capturedAt,
      note: params.note ?? "",
      sourceActor: "browser",
      mimeType: "image/png",
      bytes,
      // r2Key and presignedUrl set after upload via Worker endpoint
    };
  }
}
