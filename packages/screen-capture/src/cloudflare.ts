import type { ScreenCaptureActor, CaptureParams, ScreenshotArtifact } from "./interface.ts";

/**
 * CloudflareScreenCaptureActor — uses Cloudflare Browser Rendering API.
 *
 * Requires:
 *   - Cloudflare Worker with browser binding
 *   - wrangler.toml: [[browser]] binding named "browser"
 *
 * See: https://developers.cloudflare.com/browser-rendering/
 *
 * Usage in a Worker:
 *   const actor = new CloudflareScreenCaptureActor(env.browser);
 *   const artifact = await actor.capture({ url: "https://example.com" });
 */
export class CloudflareScreenCaptureActor implements ScreenCaptureActor {
  readonly platform = "cloudflare";

  constructor(private readonly browser: any) {}

  async capture(params: CaptureParams): Promise<ScreenshotArtifact> {
    if (!params.url) {
      throw new Error(
        "CloudflareScreenCaptureActor requires params.url — headless capture of a specific page"
      );
    }

    const id = `shot-${Date.now()}`;
    const capturedAt = new Date().toISOString();
    const note = params.note ?? "";

    // Launch browser page via Cloudflare Browser Rendering API
    const page = await this.browser.newPage();

    if (params.viewport) {
      await page.setViewport(params.viewport);
    }

    await page.goto(params.url, { waitUntil: "networkidle0" });

    // Take screenshot as PNG — Puppeteer returns Buffer in Node, Uint8Array in Workers
    const screenshotBuffer: Uint8Array = await page.screenshot({
      type: "png",
      fullPage: !params.selector,
      ...(params.selector
        ? { clip: await page.$eval(params.selector, (el: Element) => el.getBoundingClientRect()) }
        : {}),
    });

    await page.close();

    const timestamp = new Date().toISOString().replace(/[:.]/g, "-").slice(0, 19);
    const filename = note
      ? `screenshot_${timestamp}_${note}.png`
      : `screenshot_${timestamp}.png`;

    const r2Key = `screenshots/${capturedAt.slice(0, 4)}/${capturedAt.slice(5, 7)}/${capturedAt.slice(8, 10)}/${filename}`;

    return {
      id,
      r2Bucket: "knowledge",
      r2Account: "8d78f1135e2ebd70b5c8f5dee9d519ff",
      capturedAt,
      note,
      sourceActor: "cloudflare",
      mimeType: "image/png",
      bytes: screenshotBuffer.byteLength,
      r2Key,
      // presignedUrl set after upload to R2
    };
  }
}
