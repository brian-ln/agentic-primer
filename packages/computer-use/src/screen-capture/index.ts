export type { ScreenCaptureActor, CaptureParams, ScreenshotArtifact } from "./interface.ts";
export { CancelledError, detectPlatform } from "./interface.ts";
export { NativeScreenCaptureActor } from "./native/index.ts";
export { MacOSScreenCaptureActor } from "./native/macos.ts";
export { WindowsScreenCaptureActor } from "./native/windows.ts";
export { LinuxScreenCaptureActor } from "./native/linux.ts";
export { BrowserScreenCaptureActor } from "./browser.ts";
export { CloudflareScreenCaptureActor } from "./cloudflare.ts";
