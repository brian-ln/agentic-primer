import { createCozoClient } from "./cozo-wasm-client";

async function test() {
  const db = await createCozoClient();

  // Test 1: Simple schema
  console.log("Test 1: Simple schema without keys");
  try {
    await db.run(":create test1 {x: Int, y: String}");
    console.log("✓ Works\n");
  } catch (e) {
    console.log("✗ Failed:", (e as Error).message, "\n");
  }

  // Test 2: Schema with default keys syntax
  console.log("Test 2: Default keys (no explicit specification)");
  try {
    await db.run(":create test2 {id: String, name: String}");
    console.log("✓ Works\n");
  } catch (e) {
    console.log("✗ Failed:", (e as Error).message, "\n");
  }

  // Test 3: Insert and check what the actual key is
  console.log("Test 3: What are the default keys?");
  await db.run("?[id, name] <- [['a', 'Alice']] :put test2 {id, name}");
  await db.run("?[id, name] <- [['a', 'Alice']] :put test2 {id, name}");
  const result = await db.run("?[id, name] := *test2{id, name}");
  console.log("Rows after 2x insert of same data:", result.rows);
  console.log("(If 2 rows: no key constraint, if 1 row: has key)\n");

  db.close();
}

test().catch(console.error);
