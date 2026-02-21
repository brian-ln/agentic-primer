/// <reference types="bun-types" />
import { mkdir } from "node:fs/promises";
import type { ScreenCaptureActor, CaptureParams, ScreenshotArtifact } from "../interface.ts";
import { CancelledError } from "../interface.ts";

const SAVE_DIR = `${process.env.HOME}/knowledge/screenshots`;
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

async function which(cmd: string): Promise<boolean> {
  const proc = Bun.spawn(["which", cmd], { stdout: "pipe", stderr: "pipe" });
  return (await proc.exited) === 0;
}

/**
 * LinuxScreenCaptureActor — captures screenshots on Linux.
 *
 * Tool preference order (first available wins):
 *   1. flameshot   — best cross-desktop, supports region selection GUI
 *   2. maim        — modern scrot replacement, supports region selection
 *   3. scrot       — lightweight, supports region selection
 *   4. gnome-screenshot — GNOME desktop tool
 *   5. import      — ImageMagick, requires X display
 *
 * Install one of the above tools for screen capture to work.
 */
export class LinuxScreenCaptureActor implements ScreenCaptureActor {
  readonly platform = "linux";

  async capture(params: CaptureParams): Promise<ScreenshotArtifact> {
    await mkdir(SAVE_DIR, { recursive: true });

    const timestamp = buildTimestamp();
    const note = params.note ? sanitizeNote(params.note) : "";
    const filename = note
      ? `screenshot_${timestamp}_${note}.png`
      : `screenshot_${timestamp}.png`;
    const filepath = `${SAVE_DIR}/${filename}`;

    const mode = params.mode ?? "interactive";

    let args: string[];

    if (await which("flameshot")) {
      // flameshot: best cross-desktop option, supports region selection GUI
      if (mode === "region" || mode === "interactive") {
        args = ["flameshot", "gui", "--path", filepath];
      } else {
        args = ["flameshot", "full", "--path", filepath];
      }
    } else if (await which("maim")) {
      // maim: modern scrot replacement
      if (mode === "region" || mode === "interactive") {
        args = ["maim", "-s", filepath]; // -s for region selection
      } else if (mode === "window") {
        args = ["maim", "-i", "$(xdotool getactivewindow)", filepath];
      } else {
        args = ["maim", filepath];
      }
    } else if (await which("scrot")) {
      if (mode === "region" || mode === "interactive") {
        args = ["scrot", "-s", filepath];
      } else if (mode === "window") {
        args = ["scrot", "-u", filepath]; // -u = focused window
      } else {
        args = ["scrot", filepath];
      }
    } else if (await which("gnome-screenshot")) {
      if (mode === "region" || mode === "interactive") {
        args = ["gnome-screenshot", "-a", "-f", filepath];
      } else if (mode === "window") {
        args = ["gnome-screenshot", "-w", "-f", filepath];
      } else {
        args = ["gnome-screenshot", "-f", filepath];
      }
    } else if (await which("import")) {
      // ImageMagick import — requires X display
      if (mode === "fullscreen") {
        args = ["import", "-window", "root", filepath];
      } else {
        args = ["import", filepath]; // click and drag for region
      }
    } else {
      throw new Error(
        "LinuxScreenCaptureActor: no supported screenshot tool found. " +
        "Install one of: flameshot, maim, scrot, gnome-screenshot, or imagemagick."
      );
    }

    const proc = Bun.spawn(args);
    const exitCode = await proc.exited;

    const file = Bun.file(filepath);
    if (exitCode !== 0 || !(await file.exists())) {
      throw new CancelledError("Screen capture cancelled or failed");
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
