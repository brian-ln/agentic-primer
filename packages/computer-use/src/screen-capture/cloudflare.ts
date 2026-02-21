import type { ScreenCaptureActor, CaptureParams, ScreenshotArtifact } from "./interface.ts";

/**
 * CloudflareScreenCaptureActor — uses Cloudflare Browser Rendering API.
 *
 * Requires:
 *   - Cloudflare Worker with browser binding
 *   - wrangler.toml: [[browser]] binding named "browser" (BrowserWorker)
 *
 * See: https://developers.cloudflare.com/browser-rendering/
 *
 * Usage in a Worker:
 *   const actor = new CloudflareScreenCaptureActor(env.browser);
 *   const artifact = await actor.capture({ url: "https://example.com" });
 *
 * Notes:
 *   - Only "fullscreen" mode is supported (no interactive/window/region in headless).
 *   - `data` on the returned artifact contains the raw PNG bytes.
 *   - `r2Key` is null — upload to R2 is handled by the Worker endpoint, not here.
 *   - `localPath` is null — no filesystem in Workers.
 */
export class CloudflareScreenCaptureActor implements ScreenCaptureActor {
  readonly platform = "cloudflare";

  constructor(private readonly browserBinding: unknown) {}

  async capture(params: CaptureParams): Promise<ScreenshotArtifact> {
    if (!params.url) {
      throw new Error(
        "CloudflareScreenCaptureActor requires a url in CaptureParams"
      );
    }

    const { mode } = params;
    if (mode === "interactive" || mode === "window" || mode === "region") {
      throw new Error(
        "CloudflareScreenCaptureActor only supports fullscreen mode"
      );
    }

    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const puppeteer = await import("@cloudflare/puppeteer") as any;
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const browser = await puppeteer.launch(this.browserBinding as any);

    try {
      const page = await browser.newPage();

      await page.setViewport(
        params.viewport ?? { width: 1280, height: 720 }
      );

      await page.goto(params.url, { waitUntil: "networkidle0" });

      let screenshotData: Buffer | Uint8Array;

      if (params.selector) {
        const element = await page.$(params.selector);
        if (!element) {
          throw new Error(`Selector not found: ${params.selector}`);
        }
        screenshotData = await element.screenshot({ type: "png" }) as Buffer;
      } else {
        screenshotData = await page.screenshot({
          type: "png",
          fullPage: true,
        }) as Buffer;
      }

      const id = `shot-${Date.now()}`;
      const capturedAt = new Date().toISOString();
      const note = params.note ?? "";

      const data =
        screenshotData instanceof Uint8Array
          ? screenshotData
          : new Uint8Array(screenshotData);

      return {
        id,
        capturedAt,
        note,
        localPath: null,
        r2Key: null,
        r2Bucket: "knowledge",
        r2Account: "8d78f1135e2ebd70b5c8f5dee9d519ff",
        sourceActor: "cloudflare",
        mimeType: "image/png",
        bytes: data.byteLength,
        data,
      };
    } finally {
      await browser.close();
    }
  }
}
