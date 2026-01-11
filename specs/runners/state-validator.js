#!/usr/bin/env node
/**
 * State Machine Validator - Validate state transitions at runtime
 * Simplified version - loads specs and reports on validation
 */

import { readFileSync, readdirSync } from 'fs';
import { join, dirname } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));

function parseStateMachine(markdown) {
  const lines = markdown.split('\n');
  const spec = { name: '', states: [], transitions: [] };
  
  for (const line of lines) {
    const trimmed = line.trim();
    if (trimmed.startsWith('# ') && !spec.name) {
      spec.name = trimmed.substring(2);
    } else if (trimmed.startsWith('| ') && trimmed.includes(' -> ')) {
      const parts = trimmed.split('|').map(p => p.trim()).filter(p => p);
      if (parts.length >= 3 && parts[0] !== 'From' && parts[0] !== '---') {
        spec.transitions.push({ from: parts[0], event: parts[1], to: parts[2] });
        if (!spec.states.includes(parts[0])) spec.states.push(parts[0]);
        if (!spec.states.includes(parts[2])) spec.states.push(parts[2]);
      }
    }
  }
  return spec;
}

async function validateStateMachine(specPath) {
  const content = readFileSync(specPath, 'utf8');
  const spec = parseStateMachine(content);
  
  console.log(`\nState Machine: ${spec.name}`);
  console.log(`  States: ${spec.states.length}`);
  console.log(`  Transitions: ${spec.transitions.length}`);
  console.log(`  Status: ✗ Runtime validation not implemented`);
  
  return { spec: spec.name, validated: false, reason: 'Not implemented' };
}

async function main() {
  const statesDir = join(__dirname, '../state-machines');
  console.log('State Machine Validator');
  
  const files = readdirSync(statesDir).filter(f => f.endsWith('.md'));
  console.log(`Found ${files.length} state machine spec(s)`);
  
  let validated = 0, failed = 0;
  for (const file of files) {
    const result = await validateStateMachine(join(statesDir, file));
    if (result.validated) validated++; else failed++;
  }
  
  console.log(`\n${'='.repeat(60)}`);
  console.log(`Validated: ${validated}`);
  console.log(`Failed: ${failed}`);
  process.exit(failed > 0 ? 1 : 0);
}

if (import.meta.url === `file://${process.argv[1]}`) {
  main().catch(console.error);
}

export { parseStateMachine, validateStateMachine };
