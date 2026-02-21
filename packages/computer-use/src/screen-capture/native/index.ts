/// <reference types="bun-types" />
import type { ScreenCaptureActor, CaptureParams, ScreenshotArtifact } from "../interface.ts";

/**
 * NativeScreenCaptureActor — delegates to the appropriate platform actor
 * based on process.platform at runtime.
 *
 * Supported platforms:
 *   darwin  → MacOSScreenCaptureActor  (screencapture CLI)
 *   win32   → WindowsScreenCaptureActor (PowerShell)
 *   linux   → LinuxScreenCaptureActor  (flameshot/maim/scrot/gnome-screenshot/import)
 */
export class NativeScreenCaptureActor implements ScreenCaptureActor {
  readonly platform = "native";

  private async getDelegate(): Promise<ScreenCaptureActor> {
    const { platform } = process;
    if (platform === "darwin") {
      const { MacOSScreenCaptureActor } = await import("./macos.ts");
      return new MacOSScreenCaptureActor();
    }
    if (platform === "win32") {
      const { WindowsScreenCaptureActor } = await import("./windows.ts");
      return new WindowsScreenCaptureActor();
    }
    if (platform === "linux") {
      const { LinuxScreenCaptureActor } = await import("./linux.ts");
      return new LinuxScreenCaptureActor();
    }
    throw new Error(`NativeScreenCaptureActor: unsupported platform '${platform}'`);
  }

  async capture(params: CaptureParams): Promise<ScreenshotArtifact> {
    const delegate = await this.getDelegate();
    return delegate.capture(params);
  }
}
