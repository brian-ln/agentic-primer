// Simple WASM test to verify Bun native compile support
// This tests whether ANY WASM can work in compiled binaries

console.log("🧪 Simple WASM Test\n");

// Create a minimal WASM module inline (adds two numbers)
const wasmBytes = new Uint8Array([
  0x00, 0x61, 0x73, 0x6d, // Magic number \0asm
  0x01, 0x00, 0x00, 0x00, // Version 1
  // Type section
  0x01, 0x07, 0x01, // Section: type, length: 7, 1 entry
  0x60, 0x02, 0x7f, 0x7f, 0x01, 0x7f, // func (i32, i32) -> i32
  // Function section
  0x03, 0x02, 0x01, 0x00, // Section: func, length: 2, 1 func using type 0
  // Export section
  0x07, 0x07, 0x01, // Section: export, length: 7, 1 export
  0x03, 0x61, 0x64, 0x64, // "add"
  0x00, 0x00, // function index 0
  // Code section
  0x0a, 0x09, 0x01, // Section: code, length: 9, 1 function
  0x07, 0x00, // func body size: 7, 0 locals
  0x20, 0x00, // local.get 0
  0x20, 0x01, // local.get 1
  0x6a, // i32.add
  0x0b, // end
]);

async function testWasm() {
  try {
    console.log("Creating WASM module from bytes...");
    const module = await WebAssembly.compile(wasmBytes);
    const instance = await WebAssembly.instantiate(module);

    const add = instance.exports.add as (a: number, b: number) => number;
    const result = add(5, 7);

    console.log(`✓ WASM execution: 5 + 7 = ${result}`);

    if (result === 12) {
      console.log("✅ Simple WASM test passed!");
      return 0;
    } else {
      console.error(`❌ Expected 12, got ${result}`);
      return 1;
    }
  } catch (error) {
    console.error("❌ WASM test failed:");
    console.error(error);
    return 1;
  }
}

testWasm().then((exitCode) => {
  process.exit(exitCode);
});
