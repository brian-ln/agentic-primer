#!/usr/bin/env node
/**
 * FIT Runner - Execute FIT/SLIM decision tables
 * Simplified version - tables marked as "not implemented" for now
 */

import { readFileSync, readdirSync, statSync } from 'fs';
import { join, dirname } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));

function parseDecisionTable(markdown) {
  const tables = [];
  const lines = markdown.split('\n');
  let currentTable = null;
  let inTable = false;

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i].trim();
    if (!line) continue;
    if (line.startsWith('## Table')) {
      if (currentTable) tables.push(currentTable);
      currentTable = { name: line.substring(3).trim(), headers: [], rows: [] };
      inTable = false;
    } else if (line.startsWith('|') && !inTable && currentTable) {
      currentTable.headers = line.split('|').slice(1, -1).map(h => h.trim());
      inTable = true;
    } else if (line.match(/^\|[\s\-:|]+\|$/)) {
      continue;
    } else if (line.startsWith('|') && inTable && currentTable) {
      const cells = line.split('|').slice(1, -1).map(c => c.trim());
      if (cells.length === currentTable.headers.length) {
        currentTable.rows.push({ cells, line: i + 1 });
      }
    } else if (!line.startsWith('|') && inTable) {
      inTable = false;
    }
  }
  if (currentTable) tables.push(currentTable);
  return tables;
}

async function executeFixture(fixturePath) {
  const content = readFileSync(fixturePath, 'utf8');
  const tables = parseDecisionTable(content);
  const results = { fixture: fixturePath, totalRows: 0, passedRows: 0, failedRows: 0 };

  console.log(`\nFixture: ${fixturePath}`);
  for (const table of tables) {
    console.log(`  ${table.name}`);
    for (const row of table.rows) {
      results.totalRows++;
      results.failedRows++;
      console.log(`    ✗ Row ${row.line} - Not implemented`);
    }
  }
  return results;
}

function findFixtureFiles(dir) {
  const files = [];
  try {
    for (const entry of readdirSync(dir)) {
      const fullPath = join(dir, entry);
      if (statSync(fullPath).isDirectory()) {
        files.push(...findFixtureFiles(fullPath));
      } else if (entry.endsWith('.fit.md')) {
        files.push(fullPath);
      }
    }
  } catch (e) {}
  return files;
}

async function main() {
  const fixturesDir = join(__dirname, '../fit-fixtures');
  console.log('FIT Test Runner');
  const fixtureFiles = findFixtureFiles(fixturesDir);
  console.log(`Found ${fixtureFiles.length} fixture file(s)`);

  let totalPassed = 0, totalFailed = 0;
  for (const file of fixtureFiles) {
    const result = await executeFixture(file);
    totalPassed += result.passedRows;
    totalFailed += result.failedRows;
  }

  console.log(`\n${'='.repeat(60)}`);
  console.log(`Rows passed: ${totalPassed}`);
  console.log(`Rows failed: ${totalFailed}`);
  process.exit(totalFailed > 0 ? 1 : 0);
}

if (import.meta.url === `file://${process.argv[1]}`) {
  main().catch(console.error);
}

export { parseDecisionTable, executeFixture };
