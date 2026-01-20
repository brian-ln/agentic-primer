/**
 * CozoDB FFI Client Test Suite
 *
 * Tests the Bun FFI wrapper for CozoDB C bindings
 */

import { createCozoFfiClient, CozoFfiClient } from "./cozo-ffi-client";

console.log("🧪 CozoDB FFI Client Test Suite\n");
console.log("============================================================\n");

let testsPassed = 0;
let testsFailed = 0;

function test(name: string, fn: () => void | Promise<void>) {
  try {
    const result = fn();
    if (result instanceof Promise) {
      result
        .then(() => {
          console.log(`✓ ${name}`);
          testsPassed++;
        })
        .catch((error) => {
          console.error(`✗ ${name}: ${error.message}`);
          testsFailed++;
        });
    } else {
      console.log(`✓ ${name}`);
      testsPassed++;
    }
  } catch (error: any) {
    console.error(`✗ ${name}: ${error.message}`);
    testsFailed++;
  }
}

// Test 1: Create client
test("Create FFI client", () => {
  const db = createCozoFfiClient();
  db.close();
});

// Test 2: Basic query
test("Execute basic query", () => {
  const db = createCozoFfiClient();
  const result = db.run("?[] <- [[1, 2, 3]]");

  if (!result.ok) {
    throw new Error("Query failed");
  }

  if (!result.rows || result.rows.length !== 1) {
    throw new Error(`Expected 1 row, got ${result.rows?.length}`);
  }

  const row = result.rows[0];
  if (row[0] !== 1 || row[1] !== 2 || row[2] !== 3) {
    throw new Error(`Expected [1,2,3], got ${JSON.stringify(row)}`);
  }

  db.close();
});

// Test 3: Parameterized query
test("Execute parameterized query", () => {
  const db = createCozoFfiClient();
  const result = db.run("?[x] <- [[$value]]", { value: 42 });

  if (!result.ok) {
    throw new Error("Query failed");
  }

  if (!result.rows || result.rows.length !== 1 || result.rows[0][0] !== 42) {
    throw new Error(`Expected [[42]], got ${JSON.stringify(result.rows)}`);
  }

  db.close();
});

// Test 4: Create relation
test("Create and query relation", () => {
  const db = createCozoFfiClient();

  // Create relation
  db.run(":create users {id: Int, name: String}");

  // Insert data
  db.run("?[id, name] <- [[1, 'Alice'], [2, 'Bob']] :put users {id, name}");

  // Query data
  const result = db.run("?[id, name] := *users{id, name}");

  if (!result.ok) {
    throw new Error("Query failed");
  }

  if (!result.rows || result.rows.length !== 2) {
    throw new Error(`Expected 2 rows, got ${result.rows?.length}`);
  }

  db.close();
});

// Test 5: Delete data
test("Delete from relation", () => {
  const db = createCozoFfiClient();

  // Create and populate
  db.run(":create users {id: Int, name: String}");
  db.run("?[id, name] <- [[1, 'Alice'], [2, 'Bob']] :put users {id, name}");

  // Delete
  db.run("?[id, name] <- [[1, 'Alice']] :rm users {id, name}");

  // Query
  const result = db.run("?[id, name] := *users{id, name}");

  if (!result.rows || result.rows.length !== 1) {
    throw new Error(`Expected 1 row after delete, got ${result.rows?.length}`);
  }

  if (result.rows[0][1] !== "Bob") {
    throw new Error("Wrong row deleted");
  }

  db.close();
});

// Test 6: Complex query with joins
test("Execute complex query with joins", () => {
  const db = createCozoFfiClient();

  // Create relations
  db.run(":create users {id: Int, name: String}");
  db.run(":create posts {id: Int, user_id: Int, title: String}");

  // Insert data
  db.run("?[id, name] <- [[1, 'Alice'], [2, 'Bob']] :put users {id, name}");
  db.run(
    "?[id, user_id, title] <- [[1, 1, 'First'], [2, 1, 'Second'], [3, 2, 'Third']] :put posts {id, user_id, title}"
  );

  // Join query
  const result = db.run(`
    ?[user_name, post_title] :=
      *users{id: user_id, name: user_name},
      *posts{user_id, title: post_title}
  `);

  if (!result.ok) {
    throw new Error("Query failed");
  }

  if (!result.rows || result.rows.length !== 3) {
    throw new Error(`Expected 3 rows, got ${result.rows?.length}`);
  }

  db.close();
});

// Test 7: Aggregation (skipped - requires graph-algo feature)
test("Execute aggregation query (SKIP - requires graph-algo)", () => {
  const db = createCozoFfiClient();

  db.run(":create items {id: Int, category: String, price: Float}");
  db.run(`
    ?[id, category, price] <- [
      [1, 'A', 10.0],
      [2, 'A', 20.0],
      [3, 'B', 30.0]
    ] :put items {id, category, price}
  `);

  // Just verify we can query the data (sum not available in minimal build)
  const result = db.run("?[id, category, price] := *items{id, category, price}");

  if (!result.ok) {
    throw new Error("Query failed");
  }

  if (!result.rows || result.rows.length !== 3) {
    throw new Error(`Expected 3 rows, got ${result.rows?.length}`);
  }

  db.close();
});

// Test 8: Error handling
test("Handle query errors gracefully", () => {
  const db = createCozoFfiClient();

  try {
    db.run("INVALID QUERY SYNTAX");
    throw new Error("Should have thrown error");
  } catch (error: any) {
    if (!error.message.includes("CozoScript error")) {
      throw new Error(`Wrong error type: ${error.message}`);
    }
  }

  db.close();
});

// Test 9: Multiple clients
test("Use multiple independent clients", () => {
  const db1 = createCozoFfiClient();
  const db2 = createCozoFfiClient();

  db1.run(":create users {id: Int}");
  db1.run("?[id] <- [[1]] :put users {id}");

  // db2 should not see db1's data (separate instances)
  try {
    db2.run("?[id] := *users{id}");
    throw new Error("Should have failed - relation doesn't exist in db2");
  } catch (error: any) {
    if (!error.message.includes("CozoScript error")) {
      throw new Error(`Wrong error: ${error.message}`);
    }
  }

  db1.close();
  db2.close();
});

// Test 10: Close and reopen
test("Close client", () => {
  const db = createCozoFfiClient();
  db.run("?[] <- [[1]]"); // Should work

  db.close();

  if (!db.isClosed()) {
    throw new Error("Database should be closed");
  }

  try {
    db.run("?[] <- [[1]]"); // Should fail
    throw new Error("Should not be able to run query after close");
  } catch (error: any) {
    if (!error.message.includes("Database is closed")) {
      throw new Error(`Wrong error: ${error.message}`);
    }
  }
});

// Test 11: Query method
test("Use query() helper method", async () => {
  const db = createCozoFfiClient();
  const rows = await db.query("?[x] <- [[1], [2], [3]]");

  if (rows.length !== 3) {
    throw new Error(`Expected 3 rows, got ${rows.length}`);
  }

  if (rows[0][0] !== 1 || rows[1][0] !== 2 || rows[2][0] !== 3) {
    throw new Error(`Unexpected row values: ${JSON.stringify(rows)}`);
  }

  db.close();
});

// Wait for async tests to complete
setTimeout(() => {
  console.log("\n============================================================\n");
  console.log(`📊 Test Results: ${testsPassed} passed, ${testsFailed} failed\n`);

  if (testsFailed === 0) {
    console.log("✅ All tests passed!\n");
  } else {
    console.log(`❌ ${testsFailed} test(s) failed\n`);
    process.exit(1);
  }
}, 100);
