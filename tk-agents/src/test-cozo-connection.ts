// Test CozoDB connection
import { CozoDb } from "cozo-lib-wasm";

async function testConnection() {
  console.log("Testing CozoDB connection...");

  try {
    // Create in-memory database
    const db = new CozoDb("mem", "", {});
    console.log("✓ CozoDB initialized (in-memory)");

    // Test basic query
    const result = await db.run("?[] <- [[1, 2, 3]]");
    console.log("✓ Basic query works:", result);

    // Test SQLite backend
    const dbSqlite = new CozoDb("sqlite", "./data/test.db", {});
    console.log("✓ CozoDB initialized (SQLite)");

    const result2 = await dbSqlite.run("?[] <- [[4, 5, 6]]");
    console.log("✓ SQLite query works:", result2);

    dbSqlite.close();
    db.close();

    console.log("\n✅ All CozoDB tests passed!");
  } catch (error) {
    console.error("❌ CozoDB test failed:", error);
    throw error;
  }
}

testConnection();
