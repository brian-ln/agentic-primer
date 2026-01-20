// Test CozoDB WASM with proper initialization
// This addresses the missing init() call from Attempt 2

import init, { CozoDb } from "cozo-lib-wasm";

async function testWasm() {
  console.log("🔧 Testing cozo-lib-wasm with proper initialization...\n");

  try {
    // CRITICAL: Initialize WASM module first
    console.log("1. Initializing WASM module...");
    await init();
    console.log("✓ WASM initialized\n");

    // Create in-memory database
    console.log("2. Creating in-memory database...");
    const db = CozoDb.new();
    console.log("✓ Database created\n");

    // Test basic query
    console.log("3. Running basic query: ?[] <- [[1, 2, 3]]");
    const result = db.run("?[] <- [[1, 2, 3]]", "{}");
    console.log("✓ Query executed\n");

    console.log("Result:", result);

    // Parse result
    const parsed = JSON.parse(result);
    console.log("\nParsed result:");
    console.log("  ok:", parsed.ok);
    console.log("  rows:", parsed.rows);
    console.log("  headers:", parsed.headers);

    // Test with parameters
    console.log("\n4. Testing parameterized query...");
    const paramResult = db.run(
      "?[x] <- [[$x]]",
      JSON.stringify({ x: 42 })
    );
    const paramParsed = JSON.parse(paramResult);
    console.log("✓ Parameterized query works");
    console.log("  result:", paramParsed.rows);

    // Test table creation and querying
    console.log("\n5. Testing table operations...");

    // Create a relation
    db.run(":create test {id: Int, name: String}", "{}");
    console.log("✓ Created relation 'test'");

    // Insert data
    db.run("?[id, name] <- [[1, 'Alice'], [2, 'Bob']] :put test {id, name}", "{}");
    console.log("✓ Inserted data");

    // Query back
    const queryResult = db.run("?[id, name] := *test{id, name}", "{}");
    const queryParsed = JSON.parse(queryResult);
    console.log("✓ Queried data:");
    console.log("  rows:", queryParsed.rows);

    console.log("\n✅ SUCCESS: cozo-lib-wasm works with Bun!");
    console.log("   Integration method: WASM (embedded, no separate process)");

    return true;
  } catch (error) {
    console.error("\n❌ FAILED:", error);
    if (error instanceof Error) {
      console.error("   Message:", error.message);
      console.error("   Stack:", error.stack);
    }
    return false;
  }
}

testWasm().then((success) => {
  process.exit(success ? 0 : 1);
});
