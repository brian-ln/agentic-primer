// Test: Proper WASM embedding using Bun's file API
// Based on Bun documentation for embedding binary files

import init, { CozoDb } from "cozo-lib-wasm";
import wasmPath from "../node_modules/cozo-lib-wasm/cozo_lib_wasm_bg.wasm" with { type: "file" };
import { file } from "bun";

async function testEmbeddedWasm() {
  console.log("🧪 Testing Bun File API Approach\n");

  try {
    console.log("WASM path:", wasmPath);

    // Use Bun's file() API to load the bundled file
    const wasmBytes = await file(wasmPath).arrayBuffer();
    console.log(`✓ Loaded ${wasmBytes.byteLength} bytes`);

    await init(wasmBytes);
    console.log("✓ WASM initialized");

    const db = CozoDb.new();
    console.log("✓ Database created");

    const result = db.run("?[] <- [[1, 2, 3]]", "{}");
    const parsed = JSON.parse(result);

    console.log("✓ Query executed:", parsed.rows[0]);

    if (parsed.ok && JSON.stringify(parsed.rows[0]) === "[1,2,3]") {
      console.log("\n✅ Bun file API approach works!");
      return 0;
    } else {
      console.error("❌ Query returned unexpected result");
      return 1;
    }
  } catch (error) {
    console.error("❌ Test failed:");
    console.error(error);
    return 1;
  }
}

testEmbeddedWasm().then((exitCode) => {
  process.exit(exitCode);
});
