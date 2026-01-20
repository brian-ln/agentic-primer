// Test suite for CozoWasmNativeClient
// This test works in both normal runtime and compiled binary

import { CozoWasmNativeClient, createCozoNativeClient } from "./cozo-wasm-native";

async function runTests() {
  console.log("🧪 CozoWasmNativeClient Test Suite\n");
  console.log("=" .repeat(60) + "\n");

  let passCount = 0;
  let failCount = 0;

  const test = async (name: string, fn: () => Promise<void>) => {
    try {
      await fn();
      console.log(`✓ ${name}`);
      passCount++;
    } catch (error) {
      console.error(`✗ ${name}`);
      console.error(`  Error: ${error instanceof Error ? error.message : String(error)}`);
      failCount++;
    }
  };

  // Test 1: Basic initialization
  await test("Initialize client", async () => {
    const client = new CozoWasmNativeClient();
    if (client.isReady()) {
      throw new Error("Client should not be ready before initialization");
    }
    await client.initialize();
    if (!client.isReady()) {
      throw new Error("Client should be ready after initialization");
    }
    client.close();
  });

  // Test 2: Helper function
  await test("Create client with helper", async () => {
    const client = await createCozoNativeClient();
    if (!client.isReady()) {
      throw new Error("Client should be ready");
    }
    client.close();
  });

  // Test 3: Basic query
  const client = await createCozoNativeClient();

  await test("Execute basic query", async () => {
    const result = await client.run("?[] <- [[1, 2, 3]]");
    if (!result.ok || result.rows.length !== 1) {
      throw new Error("Query failed");
    }
  });

  // Test 4: Parameterized query
  await test("Parameterized query", async () => {
    const result = await client.run("?[x] <- [[$val]]", { val: 42 });
    if (result.rows[0][0] !== 42) {
      throw new Error("Parameter not substituted");
    }
  });

  // Test 5: Create relation
  await test("Create and query relation", async () => {
    await client.run(":create test_users {id: Int, name: String}");
    await client.run("?[id, name] <- [[1, 'Alice']] :put test_users {id, name}");
    const result = await client.run("?[id, name] := *test_users{id, name}");
    if (result.rows.length !== 1) {
      throw new Error("Expected 1 row");
    }
  });

  // Test 6: Sync method
  await test("Synchronous query", async () => {
    const result = client.runSync("?[] <- [[99]]");
    if (result.rows[0][0] !== 99) {
      throw new Error("Sync query failed");
    }
  });

  // Test 7: Ping
  await test("Ping database", async () => {
    const isAlive = await client.ping();
    if (!isAlive) {
      throw new Error("Ping failed");
    }
  });

  // Test 8: Error handling
  await test("Handle errors gracefully", async () => {
    try {
      await client.run("INVALID SYNTAX");
      throw new Error("Should have thrown");
    } catch (error) {
      if (!(error instanceof Error) || !error.message.includes("CozoDB query failed")) {
        throw error;
      }
    }
  });

  // Cleanup
  client.close();

  // Summary
  console.log("\n" + "=".repeat(60));
  console.log(`\n📊 Results: ${passCount} passed, ${failCount} failed`);

  if (failCount === 0) {
    console.log("\n✅ All tests passed!");
    return 0;
  } else {
    console.log(`\n❌ ${failCount} test(s) failed`);
    return 1;
  }
}

runTests().then((exitCode) => {
  process.exit(exitCode);
});
