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

export class MacOSScreenCaptureActor implements ScreenCaptureActor {
  readonly platform = "macos";

  async capture(params: CaptureParams): Promise<ScreenshotArtifact> {
    await mkdir(SAVE_DIR, { recursive: true });

    const timestamp = buildTimestamp();
    const note = params.note ? sanitizeNote(params.note) : "";
    const filename = note
      ? `screenshot_${timestamp}_${note}.png`
      : `screenshot_${timestamp}.png`;
    const filepath = `${SAVE_DIR}/${filename}`;

    // Map CaptureParams.mode to screencapture flags
    const modeFlag: string | null =
      params.mode === "window"
        ? "-w"
        : params.mode === "region"
        ? "-s"
        : params.mode === "fullscreen"
        ? null  // no flag = fullscreen
        : "-i"; // interactive (default)

    const args = ["screencapture"];
    if (modeFlag !== null) args.push(modeFlag);
    args.push("-t", "png", filepath);

    const proc = Bun.spawn(args);
    await proc.exited;

    const file = Bun.file(filepath);
    if (!(await file.exists())) {
      throw new CancelledError("Screen capture cancelled by user");
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
