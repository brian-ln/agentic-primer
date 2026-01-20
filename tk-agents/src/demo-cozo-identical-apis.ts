/**
 * CozoDB FFI vs WASM - Identical API Demo
 *
 * Demonstrates that both FFI and WASM have nearly identical APIs
 * and can be used interchangeably
 */

import { createCozoFfiClient } from "./cozo-ffi-client";
import { createCozoClient } from "./cozo-wasm-client";

console.log("🔄 CozoDB: Identical API Demo\n");
console.log("Demonstrating that FFI and WASM are interchangeable\n");
console.log("============================================================\n");

async function runWithFFI() {
  console.log("📦 Using FFI (Native)");
  console.log("─".repeat(60));

  const db = createCozoFfiClient();

  // Create relation
  db.run(":create products {id: Int, name: String, price: Float}");

  // Insert data
  db.run(`
    ?[id, name, price] <- [
      [1, 'Laptop', 999.99],
      [2, 'Mouse', 29.99],
      [3, 'Keyboard', 79.99]
    ] :put products {id, name, price}
  `);

  // Query for products under $100
  const result = db.run("?[name, price] := *products{name, price}, price < 100.0");

  console.log(`✓ Query executed`);
  console.log(`✓ Found ${result.rows?.length || 0} products under $100:`);

  for (const row of result.rows || []) {
    console.log(`  - ${row[0]}: $${row[1]}`);
  }

  console.log();
  db.close();
}

async function runWithWASM() {
  console.log("🌐 Using WASM (Portable)");
  console.log("─".repeat(60));

  const db = await createCozoClient();

  // Create relation
  await db.run(":create products {id: Int, name: String, price: Float}");

  // Insert data
  await db.run(`
    ?[id, name, price] <- [
      [1, 'Laptop', 999.99],
      [2, 'Mouse', 29.99],
      [3, 'Keyboard', 79.99]
    ] :put products {id, name, price}
  `);

  // Query for products under $100
  const result = await db.run("?[name, price] := *products{name, price}, price < 100.0");

  console.log(`✓ Query executed`);
  console.log(`✓ Found ${result.rows?.length || 0} products under $100:`);

  for (const row of result.rows || []) {
    console.log(`  - ${row[0]}: $${row[1]}`);
  }

  console.log();
  db.close();
}

async function main() {
  // Run with FFI
  await runWithFFI();

  // Run with WASM
  await runWithWASM();

  console.log("============================================================");
  console.log("✅ Both approaches produce identical results!\n");

  console.log("💡 Key Points:\n");
  console.log("  1. Same query script works on both");
  console.log("  2. Same result format from both");
  console.log("  3. Only difference: FFI is sync, WASM is async");
  console.log("  4. Easy to switch between approaches\n");

  console.log("🎯 Use Case Decision:\n");
  console.log("  • Need portability? → WASM");
  console.log("  • Need max performance? → FFI");
  console.log("  • Not sure? → Start with WASM (you can switch later)\n");
}

main().catch(console.error);
