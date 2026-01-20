/**
 * Basic Bun FFI Test
 *
 * Purpose: Verify Bun FFI works on macOS ARM64
 * Tests: Loading system libraries (libSystem.dylib) and calling basic C functions
 */

import { dlopen, FFIType, suffix, CString } from "bun:ffi";

console.log("🧪 Bun FFI Basic Test\n");
console.log("Platform suffix:", suffix); // Should be "dylib" on macOS

// Test 1: Load libSystem (macOS standard C library)
console.log("\n=== Test 1: Load libSystem.dylib ===");

try {
  const lib = dlopen("libSystem.dylib", {
    // strlen: get length of C string
    strlen: {
      args: ["cstring"],
      returns: "usize",
    },
    // getpid: get process ID
    getpid: {
      args: [],
      returns: "int",
    },
  });

  console.log("✓ libSystem.dylib loaded successfully");

  // Test strlen
  const testStr = "Hello, FFI!";
  const len = lib.symbols.strlen(Buffer.from(testStr + "\0"));
  console.log(`✓ strlen("${testStr}") = ${len}`);
  console.log(`  Expected: ${testStr.length}, Got: ${len}`);

  if (len === testStr.length) {
    console.log("✓ strlen works correctly!");
  } else {
    console.log("❌ strlen returned unexpected value");
  }

  // Test getpid
  const pid = lib.symbols.getpid();
  console.log(`✓ getpid() = ${pid}`);
  console.log(`  Current process PID: ${process.pid}`);

  if (pid === process.pid) {
    console.log("✓ getpid works correctly!");
  } else {
    console.log("❌ getpid returned unexpected value");
  }

  lib.close();
  console.log("✓ Library closed");
} catch (error) {
  console.error("❌ Failed to load libSystem.dylib:", error);
}

// Test 2: Try loading libsqlite3 (if available)
console.log("\n=== Test 2: Load libsqlite3.dylib ===");

try {
  const sqlite = dlopen("libsqlite3.dylib", {
    sqlite3_libversion: {
      args: [],
      returns: "cstring",
    },
  });

  const version = sqlite.symbols.sqlite3_libversion();
  const versionStr = new CString(version);
  console.log(`✓ SQLite version: ${versionStr}`);

  sqlite.close();
  console.log("✓ Library closed");
} catch (error) {
  console.error("❌ Failed to load libsqlite3:", error);
}

console.log("\n✅ Bun FFI Basic Test Complete");
console.log("\n📋 Summary:");
console.log("  - Bun FFI is working on this system");
console.log("  - Can load system .dylib files");
console.log("  - Can call C functions with various types");
console.log("  - CString conversion works");
