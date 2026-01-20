// Test CozoDB HTTP client
import { CozoClient } from "./cozo-client.ts";

async function testClient() {
  console.log("Testing CozoDB HTTP client...\n");

  const client = new CozoClient("http://127.0.0.1:9070");

  try {
    // Test 1: Ping
    console.log("Test 1: Ping server");
    const pingResult = await client.ping();
    console.log(`  ✓ Ping: ${pingResult ? "SUCCESS" : "FAILED"}\n`);

    // Test 2: Simple query
    console.log("Test 2: Simple query");
    const result1 = await client.run("?[a, b, c] <- [[1, 2, 3], [4, 5, 6]]");
    console.log(`  ✓ Query succeeded: ${result1.rows.length} rows`);
    console.log(`  ✓ Result:`, result1.rows);
    console.log(`  ✓ Headers:`, result1.headers);
    console.log(`  ✓ Took: ${result1.took}ms\n`);

    // Test 3: Create a table
    console.log("Test 3: Create table");
    await client.run(`
      :create test_table {
        id: String,
        value: Int,
        => [id]
      }
    `);
    console.log("  ✓ Table created\n");

    // Test 4: Insert data
    console.log("Test 4: Insert data");
    await client.run(`
      ?[id, value] <- [["task_1", 100], ["task_2", 200]]
      :put test_table {id, value}
    `);
    console.log("  ✓ Data inserted\n");

    // Test 5: Query data
    console.log("Test 5: Query data");
    const result2 = await client.run(`
      ?[id, value] := *test_table[id, value]
    `);
    console.log(`  ✓ Query succeeded: ${result2.rows.length} rows`);
    console.log(`  ✓ Result:`, result2.rows);
    console.log();

    // Test 6: Query with filter
    console.log("Test 6: Query with filter");
    const result3 = await client.run(`
      ?[id, value] := *test_table[id, value], value > 100
    `);
    console.log(`  ✓ Query succeeded: ${result3.rows.length} rows`);
    console.log(`  ✓ Result:`, result3.rows);
    console.log();

    // Test 7: Clean up
    console.log("Test 7: Clean up");
    await client.run(":remove test_table");
    console.log("  ✓ Table removed\n");

    console.log("✅ All tests passed!");
  } catch (error) {
    console.error("❌ Test failed:", error);
    throw error;
  } finally {
    client.close();
  }
}

testClient();
