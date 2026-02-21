/// <reference types="bun-types" />
import { mkdir } from "node:fs/promises";
import type { ScreenCaptureActor, CaptureParams, ScreenshotArtifact } from "../interface.ts";

const SAVE_DIR = `${process.env.USERPROFILE ?? process.env.HOME}/knowledge/screenshots`;
const R2_BUCKET = "knowledge";
const R2_ACCOUNT = "8d78f1135e2ebd70b5c8f5dee9d519ff";

function buildTimestamp(): string {
  const now = new Date();
  const pad = (n: number, width = 2) => String(n).padStart(width, "0");
  return (
    String(now.getFullYear()) +
    pad(now.getMonth() + 1) +
    pad(now.getDate()) +
    "_" +
    pad(now.getHours()) +
    pad(now.getMinutes()) +
    pad(now.getSeconds())
  );
}

function sanitizeNote(raw: string): string {
  return raw.replace(/[^a-zA-Z0-9_-]/g, "_");
}

/**
 * WindowsScreenCaptureActor — captures screenshots on Windows via PowerShell.
 *
 * Limitations:
 *   - Only 'fullscreen' and 'interactive' modes are supported.
 *   - 'region' and 'window' modes require a GUI selection tool not available
 *     through PowerShell alone. These modes throw an error.
 *   - Uses System.Windows.Forms to capture the primary screen.
 */
export class WindowsScreenCaptureActor implements ScreenCaptureActor {
  readonly platform = "windows";

  async capture(params: CaptureParams): Promise<ScreenshotArtifact> {
    if (params.mode === "region" || params.mode === "window") {
      throw new Error(
        `WindowsScreenCaptureActor: mode '${params.mode}' requires a GUI selection tool. ` +
        `Only 'fullscreen' and 'interactive' (fullscreen fallback) are supported via PowerShell.`
      );
    }

    await mkdir(SAVE_DIR, { recursive: true });

    const timestamp = buildTimestamp();
    const note = params.note ? sanitizeNote(params.note) : "";
    const filename = note
      ? `screenshot_${timestamp}_${note}.png`
      : `screenshot_${timestamp}.png`;
    const filepath = `${SAVE_DIR}\\${filename}`;

    // PowerShell: capture primary screen using System.Windows.Forms
    const escapedPath = filepath.replace(/\\/g, "\\\\");
    const psScript = `
Add-Type -AssemblyName System.Windows.Forms
Add-Type -AssemblyName System.Drawing
$screen = [System.Windows.Forms.Screen]::PrimaryScreen.Bounds
$bmp = New-Object System.Drawing.Bitmap($screen.Width, $screen.Height)
$gfx = [System.Drawing.Graphics]::FromImage($bmp)
$gfx.CopyFromScreen($screen.Location, [System.Drawing.Point]::Empty, $screen.Size)
$bmp.Save('${escapedPath}')
$gfx.Dispose()
$bmp.Dispose()
    `.trim();

    const proc = Bun.spawn(["powershell", "-NoProfile", "-Command", psScript]);
    const exitCode = await proc.exited;

    if (exitCode !== 0) {
      throw new Error(`PowerShell screenshot failed (exit ${exitCode})`);
    }

    const file = Bun.file(filepath);
    if (!(await file.exists())) {
      throw new Error(`Screenshot file not created: ${filepath}`);
    }

    return {
      id: `shot-${timestamp}`,
      localPath: filepath,
      r2Bucket: R2_BUCKET,
      r2Account: R2_ACCOUNT,
      capturedAt: new Date().toISOString(),
      note,
      sourceActor: "local",
      mimeType: "image/png",
      bytes: file.size,
    };
  }
}
