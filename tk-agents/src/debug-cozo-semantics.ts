// Debug CozoDB semantics for update/delete/export

import { createCozoClient } from "./cozo-wasm-client";

async function debug() {
  const client = await createCozoClient();

  console.log("=== Testing Update Semantics ===\n");

  // Create table
  await client.run(":create users {id: Int, name: String}");
  console.log("✓ Created users table\n");

  // Insert initial data
  await client.run("?[id, name] <- [[1, 'Alice'], [2, 'Bob']] :put users {id, name}");
  console.log("Initial data:");
  let result = await client.run("?[id, name] := *users{id, name}");
  console.log(result.rows);
  console.log("");

  // Try updating
  console.log("Updating id=1 to 'Alice Updated'...");
  await client.run("?[id, name] <- [[1, 'Alice Updated']] :put users {id, name}");
  result = await client.run("?[id, name] := *users{id, name}");
  console.log("After update:");
  console.log(result.rows);
  console.log("");

  console.log("=== Testing Delete Semantics ===\n");

  // Try deleting with just id
  console.log("Attempting to delete with :rm users {id}...");
  try {
    await client.run("?[id] <- [[1]] :rm users {id}");
    console.log("✓ Delete succeeded");
  } catch (error) {
    console.log("✗ Delete failed:", (error as Error).message);
  }

  // Try deleting with both columns
  console.log("\nAttempting to delete with :rm users {id, name}...");
  try {
    await client.run("?[id, name] <- [[1, 'Alice Updated']] :rm users {id, name}");
    console.log("✓ Delete succeeded");
    result = await client.run("?[id, name] := *users{id, name}");
    console.log("After delete:");
    console.log(result.rows);
  } catch (error) {
    console.log("✗ Delete failed:", (error as Error).message);
  }

  console.log("\n=== Testing Export Semantics ===\n");

  // Recreate data
  await client.run(":create posts {id: Int, user_id: Int, title: String}");
  await client.run(
    "?[id, user_id, title] <- [[1, 2, 'Post 1']] :put posts {id, user_id, title}"
  );

  console.log("Exporting all relations...");
  const exportAll = client.exportRelations();
  console.log("Export all:", exportAll.substring(0, 200) + "...");
  console.log("");

  console.log("Exporting specific relations [users, posts]...");
  const exportSpecific = client.exportRelations(["users", "posts"]);
  console.log("Export specific:", exportSpecific.substring(0, 200) + "...");
  console.log("");

  const parsed = JSON.parse(exportAll);
  console.log("Parsed export keys:", Object.keys(parsed));
  console.log("");

  client.close();
}

debug().catch(console.error);
