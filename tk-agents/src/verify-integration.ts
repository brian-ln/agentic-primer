#!/usr/bin/env bun
// Final verification that CozoDB integration is production-ready

import { createCozoClient } from "./cozo-wasm-client";

async function verify() {
  console.log("🔍 CozoDB Integration Verification\n");
  console.log("=" .repeat(60) + "\n");

  const checks: { name: string; passed: boolean; details?: string }[] = [];

  const check = (name: string, passed: boolean, details?: string) => {
    checks.push({ name, passed, details });
    const icon = passed ? "✅" : "❌";
    console.log(`${icon} ${name}`);
    if (details) {
      console.log(`   ${details}`);
    }
  };

  try {
    // Check 1: Can initialize
    const db = await createCozoClient();
    check("Initialize CozoDB client", true, "WASM module loaded successfully");

    // Check 2: Can run basic query
    const result1 = await db.run("?[] <- [[1, 2, 3]]");
    check(
      "Execute basic query",
      result1.ok && result1.rows.length === 1,
      `Returned ${result1.rows.length} row(s)`
    );

    // Check 3: Can create relations
    await db.run(":create test {id: Int, value: String}");
    check("Create relations", true, "Schema created");

    // Check 4: Can insert data
    await db.run("?[id, value] <- [[1, 'test']] :put test {id, value}");
    check("Insert data", true, "Row inserted");

    // Check 5: Can query data back
    const result2 = await db.run("?[id, value] := *test{id, value}");
    check(
      "Query data",
      result2.rows.length === 1 && result2.rows[0][1] === "test",
      `Retrieved: ${JSON.stringify(result2.rows[0])}`
    );

    // Check 6: Can use parameters
    const result3 = await db.run("?[x] <- [[$param]]", { param: 42 });
    check(
      "Parameterized queries",
      result3.rows[0][0] === 42,
      `Parameter value: ${result3.rows[0][0]}`
    );

    // Check 7: Can handle errors
    let errorHandled = false;
    try {
      await db.run("INVALID SYNTAX");
    } catch (error) {
      errorHandled = error instanceof Error && error.message.includes("CozoDB query failed");
    }
    check("Error handling", errorHandled, "Errors throw properly");

    // Check 8: Can perform joins
    await db.run(":create users {id: Int, name: String}");
    await db.run(":create posts {id: Int, user_id: Int, title: String}");
    await db.run("?[id, name] <- [[1, 'Alice']] :put users {id, name}");
    await db.run("?[id, user_id, title] <- [[1, 1, 'Hello']] :put posts {id, user_id, title}");

    const result4 = await db.run(`
      ?[user_name, post_title] :=
        *users{id: user_id, name: user_name},
        *posts{user_id, title: post_title}
    `);
    check("Join queries", result4.rows.length === 1, `Joined ${result4.rows.length} row(s)`);

    // Check 9: Can perform aggregations
    const result5 = await db.run(`
      ?[user_id, count(post_id)] :=
        *posts{id: post_id, user_id}
    `);
    check(
      "Aggregation queries",
      result5.rows[0][1] === 1,
      `Count: ${result5.rows[0][1]}`
    );

    // Check 10: Can handle recursive queries
    await db.run(":create deps {from_id: Int, to_id: Int}");
    await db.run("?[from_id, to_id] <- [[1, 2], [2, 3]] :put deps {from_id, to_id}");

    const result6 = await db.run(`
      chain[x, depth] := x = 1, depth = 0
      chain[to, depth] :=
        chain[from, prev_depth],
        *deps{from_id: from, to_id: to},
        depth = prev_depth + 1

      ?[x, depth] := chain[x, depth]
    `);
    check(
      "Recursive queries",
      result6.rows.length === 3,
      `Found ${result6.rows.length} nodes in chain`
    );

    // Check 11: Can update (delete + insert)
    await db.run("?[id, name] <- [[1, 'Alice']] :rm users {id, name}");
    await db.run("?[id, name] <- [[1, 'Alice Updated']] :put users {id, name}");
    const result7 = await db.run("?[name] := *users{id: 1, name}");
    check(
      "Update pattern",
      result7.rows[0][0] === "Alice Updated",
      `Updated value: ${result7.rows[0][0]}`
    );

    // Check 12: Can delete
    await db.run("?[id, name] <- [[1, 'Alice Updated']] :rm users {id, name}");
    const result8 = await db.run("?[id, name] := *users{id, name}");
    check("Delete data", result8.rows.length === 0, "Row deleted successfully");

    // Check 13: Performance is acceptable
    const iterations = 100;
    const start = performance.now();
    for (let i = 0; i < iterations; i++) {
      await db.run("?[] <- [[1]]");
    }
    const elapsed = performance.now() - start;
    const avgMs = elapsed / iterations;

    check(
      "Performance",
      avgMs < 1.0,
      `${iterations} queries in ${elapsed.toFixed(2)}ms (${avgMs.toFixed(3)}ms/query)`
    );

    // Check 14: Can cleanup
    db.close();
    check("Resource cleanup", !db.isReady(), "Client closed successfully");

    // Check 15: Cannot use after close
    let cannotUseAfterClose = false;
    try {
      await db.run("?[] <- [[1]]");
    } catch (error) {
      cannotUseAfterClose =
        error instanceof Error && error.message.includes("not initialized");
    }
    check("Post-close safety", cannotUseAfterClose, "Queries blocked after close");
  } catch (error) {
    check("Unexpected error", false, (error as Error).message);
  }

  // Summary
  console.log("\n" + "=".repeat(60));

  const passed = checks.filter((c) => c.passed).length;
  const failed = checks.filter((c) => !c.passed).length;
  const total = checks.length;

  console.log(`\n📊 Results: ${passed}/${total} checks passed`);

  if (failed === 0) {
    console.log("\n✅ ALL CHECKS PASSED");
    console.log("\n🎉 CozoDB integration is PRODUCTION READY!");
    console.log("\nIntegration method: cozo-lib-wasm (WASM)");
    console.log("Client wrapper: src/cozo-wasm-client.ts");
    console.log("Usage: import { createCozoClient } from './cozo-wasm-client'");
    console.log("\nNo blockers. Ready for immediate use in dual-write system.\n");
    return 0;
  } else {
    console.log(`\n❌ ${failed} check(s) FAILED`);
    console.log("\nFailed checks:");
    checks
      .filter((c) => !c.passed)
      .forEach((c) => {
        console.log(`  - ${c.name}: ${c.details || "No details"}`);
      });
    console.log("");
    return 1;
  }
}

verify().then((exitCode) => {
  process.exit(exitCode);
});
