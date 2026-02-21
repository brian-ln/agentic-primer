/// <reference types="bun-types" />
import { test, expect, describe, mock } from "bun:test";
import { CancelledError } from "../interface.ts";

// ── CancelledError ────────────────────────────────────────────────────────────

describe("CancelledError", () => {
  test("is an instance of Error", () => {
    const err = new CancelledError();
    expect(err).toBeInstanceOf(Error);
  });

  test("default message", () => {
    const err = new CancelledError();
    expect(err.message).toBe("Screen capture cancelled");
  });

  test("custom message", () => {
    const err = new CancelledError("User pressed Escape");
    expect(err.message).toBe("User pressed Escape");
  });

  test("name is CancelledError", () => {
    const err = new CancelledError();
    expect(err.name).toBe("CancelledError");
  });

  test("instanceof CancelledError check works in catch block pattern", () => {
    let caught = false;
    try {
      throw new CancelledError("cancelled");
    } catch (e) {
      if (e instanceof CancelledError) {
        caught = true;
      }
    }
    expect(caught).toBe(true);
  });
});

// ── MacOSScreenCaptureActor command construction ──────────────────────────────
//
// These tests must run BEFORE platform dispatch tests that call mock.module(),
// because mock.module("./macos.ts", ...) replaces the cached real module and
// would prevent these tests from testing the real screencapture command.
//
// We mock Bun.spawn and Bun.file to capture args without actually running
// screencapture.

describe("MacOSScreenCaptureActor command construction", () => {
  // Helper: mock Bun.spawn + Bun.file, run capture, restore, return spawn args
  async function captureWithMode(
    mode: "interactive" | "window" | "region" | "fullscreen",
    fileExists = true,
  ): Promise<{ args: string[]; error?: unknown }> {
    const spawnCalls: string[][] = [];
    const origSpawn = (Bun as any).spawn;
    const origFile = (Bun as any).file;

    (Bun as any).spawn = (args: string[]) => {
      spawnCalls.push(args);
      return { exited: Promise.resolve(0) };
    };
    (Bun as any).file = (_p: string) => ({
      exists: async () => fileExists,
      size: 42,
    });

    try {
      const { MacOSScreenCaptureActor } = await import("./macos.ts");
      const actor = new MacOSScreenCaptureActor();
      await actor.capture({ mode });
      return { args: spawnCalls[0] ?? [] };
    } catch (error) {
      return { args: spawnCalls[0] ?? [], error };
    } finally {
      (Bun as any).spawn = origSpawn;
      (Bun as any).file = origFile;
    }
  }

  test("interactive mode → screencapture -i ... <path>", async () => {
    const { args } = await captureWithMode("interactive");
    expect(args[0]).toBe("screencapture");
    expect(args).toContain("-i");
    expect(args[args.length - 1]).toMatch(/\.png$/);
  });

  test("window mode → screencapture -w ... <path>", async () => {
    const { args } = await captureWithMode("window");
    expect(args[0]).toBe("screencapture");
    expect(args).toContain("-w");
    expect(args[args.length - 1]).toMatch(/\.png$/);
  });

  test("region mode → screencapture -s ... <path>", async () => {
    const { args } = await captureWithMode("region");
    expect(args[0]).toBe("screencapture");
    expect(args).toContain("-s");
    expect(args[args.length - 1]).toMatch(/\.png$/);
  });

  test("fullscreen mode → screencapture with no mode flag (-i/-w/-s)", async () => {
    const { args } = await captureWithMode("fullscreen");
    expect(args[0]).toBe("screencapture");
    expect(args).not.toContain("-i");
    expect(args).not.toContain("-w");
    expect(args).not.toContain("-s");
    expect(args[args.length - 1]).toMatch(/\.png$/);
  });

  test("screencapture args include -t png", async () => {
    const { args } = await captureWithMode("interactive");
    const tIdx = args.indexOf("-t");
    expect(tIdx).toBeGreaterThan(-1);
    expect(args[tIdx + 1]).toBe("png");
  });

  test("CancelledError thrown when file does not exist after capture", async () => {
    const { error } = await captureWithMode("interactive", false);
    expect(error).toBeInstanceOf(CancelledError);
  });
});

// ── NativeScreenCaptureActor platform dispatch ────────────────────────────────
//
// NativeScreenCaptureActor.getDelegate() reads process.platform at runtime and
// dynamically imports the platform-specific module. We test dispatch by:
// - darwin: mock Bun.spawn, verify a real capture resolves with sourceActor "local"
// - win32/linux: mock.module() + process.platform override, verify the right
//   class is used via a tracking side-effect
// - unsupported: verify the error message
//
// NOTE: mock.module() calls below permanently replace the cached modules for
// this test file. This is why command construction tests run first above.

describe("NativeScreenCaptureActor platform dispatch", () => {
  // Shared stub artifact for fake platform actors
  const stubArtifact = {
    id: "shot-test",
    localPath: "/tmp/test.png",
    r2Bucket: "knowledge",
    r2Account: "testaccount",
    capturedAt: "2026-02-21T12:00:00.000Z",
    note: "",
    sourceActor: "local" as const,
    mimeType: "image/png",
  };

  test("NativeScreenCaptureActor has platform = 'native'", async () => {
    const { NativeScreenCaptureActor } = await import("./index.ts");
    const actor = new NativeScreenCaptureActor();
    expect(actor.platform).toBe("native");
  });

  test("darwin → dispatches to MacOSScreenCaptureActor (real capture, spawn mocked)", async () => {
    // On darwin, NativeScreenCaptureActor delegates to MacOSScreenCaptureActor.
    // Mock Bun.spawn and Bun.file to prevent real screencapture, then verify
    // the result has sourceActor === "local" (set by MacOSScreenCaptureActor).
    const origSpawn = (Bun as any).spawn;
    const origFile = (Bun as any).file;

    (Bun as any).spawn = (_args: string[]) => ({ exited: Promise.resolve(0) });
    (Bun as any).file = (_p: string) => ({ exists: async () => true, size: 10 });

    try {
      const { NativeScreenCaptureActor } = await import("./index.ts");
      const actor = new NativeScreenCaptureActor();
      // Only run this on darwin where the real module can be loaded
      if (process.platform === "darwin") {
        const artifact = await actor.capture({ mode: "fullscreen" });
        expect(artifact.sourceActor).toBe("local");
        expect(artifact.r2Bucket).toBe("knowledge");
      } else {
        // On non-darwin CI, just verify the actor is created
        expect(actor.platform).toBe("native");
      }
    } finally {
      (Bun as any).spawn = origSpawn;
      (Bun as any).file = origFile;
    }
  });

  test("win32 → delegates to WindowsScreenCaptureActor", async () => {
    let constructedClass = "";

    mock.module("./windows.ts", () => ({
      WindowsScreenCaptureActor: class {
        readonly platform = "windows";
        async capture() {
          constructedClass = "WindowsScreenCaptureActor";
          return stubArtifact;
        }
      },
    }));

    const origPlatform = process.platform;
    Object.defineProperty(process, "platform", { value: "win32", configurable: true });

    try {
      const { NativeScreenCaptureActor } = await import("./index.ts");
      const actor = new NativeScreenCaptureActor();
      await actor.capture({ mode: "fullscreen" });
      expect(constructedClass).toBe("WindowsScreenCaptureActor");
    } finally {
      Object.defineProperty(process, "platform", { value: origPlatform, configurable: true });
    }
  });

  test("linux → delegates to LinuxScreenCaptureActor", async () => {
    let constructedClass = "";

    mock.module("./linux.ts", () => ({
      LinuxScreenCaptureActor: class {
        readonly platform = "linux";
        async capture() {
          constructedClass = "LinuxScreenCaptureActor";
          return stubArtifact;
        }
      },
    }));

    const origPlatform = process.platform;
    Object.defineProperty(process, "platform", { value: "linux", configurable: true });

    try {
      const { NativeScreenCaptureActor } = await import("./index.ts");
      const actor = new NativeScreenCaptureActor();
      await actor.capture({ mode: "fullscreen" });
      expect(constructedClass).toBe("LinuxScreenCaptureActor");
    } finally {
      Object.defineProperty(process, "platform", { value: origPlatform, configurable: true });
    }
  });

  test("unsupported platform → throws Error with platform name", async () => {
    const origPlatform = process.platform;
    Object.defineProperty(process, "platform", { value: "freebsd", configurable: true });

    try {
      const { NativeScreenCaptureActor } = await import("./index.ts");
      const actor = new NativeScreenCaptureActor();
      await expect(actor.capture({ mode: "fullscreen" })).rejects.toThrow(
        "NativeScreenCaptureActor: unsupported platform 'freebsd'"
      );
    } finally {
      Object.defineProperty(process, "platform", { value: origPlatform, configurable: true });
    }
  });
});
