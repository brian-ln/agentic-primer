/// <reference types="bun-types" />

/**
 * ScreenCaptureActor — platform-agnostic screenshot interface
 *
 * Implementations:
 *   NativeScreenCaptureActor      — delegates to MacOS/Windows/Linux by process.platform
 *   MacOSScreenCaptureActor       — macOS screencapture CLI (Bun)
 *   WindowsScreenCaptureActor     — PowerShell + System.Windows.Forms (Bun)
 *   LinuxScreenCaptureActor       — flameshot/maim/scrot/gnome-screenshot/import fallback chain
 *   BrowserScreenCaptureActor     — getDisplayMedia + canvas (browser)
 *   CloudflareScreenCaptureActor  — Browser Rendering API (Cloudflare Workers)
 */

export interface CaptureParams {
  mode?: "interactive" | "window" | "region" | "fullscreen";
  note?: string;
  upload?: boolean;
  url?: string;           // for headless capture of a specific URL
  selector?: string;      // for element-level capture (headless)
  viewport?: { width: number; height: number };
}

export interface ScreenshotArtifact {
  id: string;             // "shot-<timestamp>"
  localPath?: string | null;  // filesystem path (local actors only); null in Workers
  r2Key?: string | null;  // "screenshots/YYYY/MM/DD/<filename>.png"; null before upload
  r2Bucket: string;       // "knowledge"
  r2Account: string;      // Cloudflare account ID
  capturedAt: string;     // ISO8601 UTC
  note: string;           // "" if none
  sourceActor: "local" | "browser" | "cloudflare";
  presignedUrl?: string;  // 24h expiry
  mimeType: string;       // "image/png"
  bytes?: number;         // file size if known
  data?: Uint8Array;      // raw PNG bytes (Cloudflare Workers / headless actors)
}

export interface ScreenCaptureActor {
  readonly platform: string;
  capture(params: CaptureParams): Promise<ScreenshotArtifact>;
}

/**
 * Thrown when the user cancels an interactive screen capture
 * (e.g. presses Escape during selection).
 */
export class CancelledError extends Error {
  constructor(message = "Screen capture cancelled") {
    super(message);
    this.name = "CancelledError";
  }
}

/**
 * Runtime platform detection — returns the appropriate actor type name.
 * Actual actor instances are created by caller to avoid importing platform-
 * specific deps in all environments.
 */
export function detectPlatform(): "local" | "browser" | "cloudflare" {
  // Cloudflare Workers: no process.versions.node, CF-specific globals
  if (typeof caches !== "undefined" && typeof navigator === "undefined") {
    return "cloudflare";
  }
  // Browser: window + navigator.mediaDevices
  if (typeof window !== "undefined" && typeof navigator !== "undefined") {
    return "browser";
  }
  // Bun/Node local
  return "local";
}
