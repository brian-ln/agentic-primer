#!/usr/bin/env node
/**
 * BDD Runner - Execute Gherkin .feature files against actor implementations
 *
 * Parses Given/When/Then steps and executes them against real actors
 */

import { readFileSync, readdirSync, statSync } from 'fs';
import { join, dirname } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));

// ANSI color codes for terminal output
const colors = {
  reset: '\x1b[0m',
  bright: '\x1b[1m',
  dim: '\x1b[2m',
  red: '\x1b[31m',
  green: '\x1b[32m',
  yellow: '\x1b[33m',
  blue: '\x1b[34m',
  cyan: '\x1b[36m',
};

/**
 * Parse a .feature file into structured scenarios
 */
function parseFeatureFile(content, filePath) {
  const lines = content.split('\n');
  const feature = {
    file: filePath,
    name: '',
    description: '',
    background: [],
    scenarios: []
  };

  let currentScenario = null;
  let currentSection = null;
  let inBackground = false;

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i].trim();

    // Skip empty lines and comments
    if (!line || line.startsWith('#')) continue;

    // Feature line
    if (line.startsWith('Feature:')) {
      feature.name = line.substring(8).trim();
      currentSection = 'description';
      continue;
    }

    // Background section
    if (line.startsWith('Background:')) {
      inBackground = true;
      currentSection = 'background';
      continue;
    }

    // Scenario line
    if (line.startsWith('Scenario:')) {
      inBackground = false;
      currentScenario = {
        name: line.substring(9).trim(),
        steps: [],
        line: i + 1
      };
      feature.scenarios.push(currentScenario);
      currentSection = 'scenario';
      continue;
    }

    // Step lines (Given/When/Then/And/But)
    const stepMatch = line.match(/^(Given|When|Then|And|But)\s+(.+)$/);
    if (stepMatch) {
      const [, keyword, text] = stepMatch;
      const step = { keyword, text, line: i + 1 };

      if (inBackground) {
        feature.background.push(step);
      } else if (currentScenario) {
        currentScenario.steps.push(step);
      }
      continue;
    }

    // Description text (multi-line after Feature)
    if (currentSection === 'description' && line) {
      feature.description += (feature.description ? '\n' : '') + line;
    }
  }

  return feature;
}

/**
 * Step executor - maps Given/When/Then steps to actor operations
 */
class StepExecutor {
  constructor() {
    this.context = {};
    this.actors = new Map();
  }

  /**
   * Import actor class dynamically
   */
  async importActor(actorName) {
    const actorPath = join(__dirname, '../../src/actors', `${actorName}.js`);
    try {
      const module = await import(actorPath);
      return module;
    } catch (error) {
      throw new Error(`Failed to import actor ${actorName}: ${error.message}`);
    }
  }

  /**
   * Execute a single step
   */
  async executeStep(step, feature) {
    const { keyword, text } = step;

    try {
      // Parse step text to extract parameters
      const parsed = this.parseStepText(text);

      // Execute step based on pattern matching
      await this.matchAndExecute(keyword, parsed, feature);

      return { passed: true };
    } catch (error) {
      return {
        passed: false,
        error: error.message,
        stack: error.stack
      };
    }
  }

  /**
   * Parse step text to extract quoted strings, numbers, and JSON
   */
  parseStepText(text) {
    const result = {
      text: text,
      strings: [],
      numbers: [],
      json: null
    };

    // Extract quoted strings
    const stringMatches = text.match(/"([^"]*)"/g);
    if (stringMatches) {
      result.strings = stringMatches.map(s => s.slice(1, -1));
    }

    // Extract numbers
    const numberMatches = text.match(/\b(\d+)\b/g);
    if (numberMatches) {
      result.numbers = numberMatches.map(Number);
    }

    // Extract JSON objects
    const jsonMatch = text.match(/\{[^}]+\}/);
    if (jsonMatch) {
      try {
        // Fix single quotes to double quotes for valid JSON
        const jsonStr = jsonMatch[0].replace(/'/g, '"');
        result.json = JSON.parse(jsonStr);
      } catch (e) {
        // Not valid JSON, ignore
      }
    }

    return result;
  }

  /**
   * Match step text to implementation and execute
   */
  async matchAndExecute(keyword, parsed, feature) {
    const text = parsed.text.toLowerCase();

    // === Background / Setup Steps ===

    if (text.includes('clean test environment')) {
      this.context = { temp: {} };
      return;
    }

    if (text.includes('temporary event log file')) {
      this.context.logFile = `test-${Date.now()}.jsonl`;
      return;
    }

    // === Actor Creation Steps ===

    if (text.includes('eventlogactor')) {
      const actorModule = await this.importActor('event-log');
      this.context.actor = new actorModule.EventLogActor({
        eventLog: {
          file: this.context.logFile || 'test-events.jsonl',
          checkpointInterval: 5
        }
      });
      return;
    }

    if (text.includes('functionregistryactor')) {
      const actorModule = await this.importActor('function-registry');
      this.context.actor = new actorModule.FunctionRegistryActor();
      return;
    }

    if (text.includes('functionexecutoractor')) {
      const actorModule = await this.importActor('function-executor');
      this.context.actor = new actorModule.FunctionExecutorActor({});
      return;
    }

    // === Actor Lifecycle Steps ===

    if (text.includes('start the actor')) {
      this.context.result = await this.context.actor.start();
      return;
    }

    if (text.includes('stop the actor')) {
      this.context.result = await this.context.actor.stop();
      return;
    }

    if (text.includes('actor that is running')) {
      if (!this.context.actor.isInitialized) {
        await this.context.actor.start();
      }
      return;
    }

    if (text.includes('get the actor status')) {
      this.context.status = this.context.actor.getStatus();
      return;
    }

    // === Event Append Steps ===

    if (text.includes('append an event')) {
      const eventType = parsed.strings[0] || 'test.event';
      const eventData = parsed.json || { test: true };

      this.context.result = await this.context.actor.appendEvent({
        type: eventType,
        data: eventData
      });
      return;
    }

    if (text.includes('append') && text.includes('events')) {
      const count = parsed.numbers[0] || 5;
      this.context.results = [];

      for (let i = 0; i < count; i++) {
        const result = await this.context.actor.appendEvent({
          type: `test.event.${i}`,
          data: { index: i }
        });
        this.context.results.push(result);
      }
      return;
    }

    // === Query Steps ===

    if (text.includes('query events')) {
      const options = {};

      if (text.includes('without filters')) {
        // No filters
      } else if (text.includes('filter')) {
        const filterType = parsed.strings[0];
        if (filterType) {
          options.filter = (event) => event.type === filterType;
        }
      }

      if (parsed.numbers.length > 0) {
        options.limit = parsed.numbers[0];
      }

      this.context.queryResult = await this.context.actor.queryEvents(options);
      return;
    }

    // === Assertion Steps ===

    if (text.includes('should be initialized')) {
      if (!this.context.actor.isInitialized) {
        throw new Error('Actor is not initialized');
      }
      return;
    }

    if (text.includes('should indicate success')) {
      if (!this.context.result?.success) {
        throw new Error('Result does not indicate success');
      }
      return;
    }

    if (text.includes('should succeed')) {
      const result = this.context.result || this.context.queryResult;
      if (!result?.success) {
        throw new Error('Operation did not succeed');
      }
      return;
    }

    if (text.includes('event count should be')) {
      const expectedCount = parsed.numbers[0];
      const actualCount = this.context.actor.eventCount;
      if (actualCount !== expectedCount) {
        throw new Error(`Expected event count ${expectedCount}, got ${actualCount}`);
      }
      return;
    }

    if (text.includes('should have') && text.includes('event')) {
      // Generic event presence check
      if (!this.context.result?.eventId && !this.context.queryResult?.events?.length) {
        throw new Error('Expected events but none found');
      }
      return;
    }

    if (text.includes('should include')) {
      // Generic field presence check
      const field = parsed.strings[0];
      const target = this.context.result || this.context.status || this.context.queryResult;
      if (!target || target[field] === undefined) {
        throw new Error(`Expected field '${field}' not found in result`);
      }
      return;
    }

    // === Default: Step not implemented ===
    throw new Error(`Step not implemented: ${keyword} ${parsed.text}`);
  }

  /**
   * Clean up resources
   */
  async cleanup() {
    // Stop any running actors
    for (const [name, actor] of this.actors.entries()) {
      try {
        if (actor.stop) {
          await actor.stop();
        }
      } catch (error) {
        console.error(`Error stopping actor ${name}:`, error.message);
      }
    }

    // Clean up current actor
    if (this.context.actor) {
      try {
        if (this.context.actor.stop) {
          await this.context.actor.stop();
        }
      } catch (error) {
        console.error('Error stopping context actor:', error.message);
      }
    }

    // Clean up temp files
    if (this.context.logFile) {
      try {
        const fs = await import('fs/promises');
        await fs.unlink(this.context.logFile);
      } catch (error) {
        // Ignore cleanup errors
      }
    }
  }
}

/**
 * Execute a single scenario
 */
async function executeScenario(scenario, feature, background = []) {
  const executor = new StepExecutor();
  const results = {
    name: scenario.name,
    passed: true,
    steps: [],
    error: null
  };

  try {
    // Execute background steps first
    for (const step of background) {
      const result = await executor.executeStep(step, feature);
      if (!result.passed) {
        results.passed = false;
        results.error = `Background step failed: ${step.keyword} ${step.text}`;
        return results;
      }
    }

    // Execute scenario steps
    for (const step of scenario.steps) {
      const result = await executor.executeStep(step, feature);
      results.steps.push({
        keyword: step.keyword,
        text: step.text,
        passed: result.passed,
        error: result.error
      });

      if (!result.passed) {
        results.passed = false;
        results.error = result.error;
        break;
      }
    }
  } catch (error) {
    results.passed = false;
    results.error = error.message;
  } finally {
    await executor.cleanup();
  }

  return results;
}

/**
 * Execute all scenarios in a feature
 */
async function executeFeature(featurePath) {
  const content = readFileSync(featurePath, 'utf8');
  const feature = parseFeatureFile(content, featurePath);

  const results = {
    feature: feature.name,
    file: featurePath,
    passed: 0,
    failed: 0,
    scenarios: []
  };

  console.log(`\n${colors.bright}${colors.blue}Feature: ${feature.name}${colors.reset}`);
  console.log(`${colors.dim}${featurePath}${colors.reset}\n`);

  for (const scenario of feature.scenarios) {
    const result = await executeScenario(scenario, feature, feature.background);
    results.scenarios.push(result);

    if (result.passed) {
      results.passed++;
      console.log(`  ${colors.green}✓${colors.reset} ${scenario.name}`);
    } else {
      results.failed++;
      console.log(`  ${colors.red}✗${colors.reset} ${scenario.name}`);
      console.log(`    ${colors.red}${result.error}${colors.reset}`);
    }
  }

  return results;
}

/**
 * Find all .feature files in a directory
 */
function findFeatureFiles(dir) {
  const files = [];

  try {
    const entries = readdirSync(dir);

    for (const entry of entries) {
      const fullPath = join(dir, entry);
      const stat = statSync(fullPath);

      if (stat.isDirectory()) {
        files.push(...findFeatureFiles(fullPath));
      } else if (entry.endsWith('.feature')) {
        files.push(fullPath);
      }
    }
  } catch (error) {
    console.error(`Error reading directory ${dir}:`, error.message);
  }

  return files;
}

/**
 * Main execution
 */
async function main() {
  const featuresDir = join(__dirname, '../features');

  console.log(`${colors.bright}BDD Test Runner${colors.reset}`);
  console.log(`${colors.dim}Executing Gherkin scenarios against actors${colors.reset}`);
  console.log(`${colors.dim}Features directory: ${featuresDir}${colors.reset}`);

  const featureFiles = findFeatureFiles(featuresDir);

  if (featureFiles.length === 0) {
    console.log(`\n${colors.yellow}No .feature files found${colors.reset}`);
    process.exit(0);
  }

  console.log(`\nFound ${featureFiles.length} feature file(s)\n`);

  const allResults = [];
  let totalPassed = 0;
  let totalFailed = 0;

  for (const featureFile of featureFiles) {
    try {
      const result = await executeFeature(featureFile);
      allResults.push(result);
      totalPassed += result.passed;
      totalFailed += result.failed;
    } catch (error) {
      console.error(`${colors.red}Error executing ${featureFile}:${colors.reset}`, error.message);
      totalFailed++;
    }
  }

  // Summary
  console.log(`\n${'='.repeat(60)}`);
  console.log(`${colors.bright}Summary${colors.reset}`);
  console.log(`${'='.repeat(60)}`);
  console.log(`Features executed: ${allResults.length}`);
  console.log(`Scenarios passed: ${colors.green}${totalPassed}${colors.reset}`);
  console.log(`Scenarios failed: ${colors.red}${totalFailed}${colors.reset}`);
  console.log(`Success rate: ${totalPassed + totalFailed > 0 ? Math.round(totalPassed / (totalPassed + totalFailed) * 100) : 0}%`);

  // Exit with error code if any scenarios failed
  process.exit(totalFailed > 0 ? 1 : 0);
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  main().catch(error => {
    console.error('Fatal error:', error);
    process.exit(1);
  });
}

export { parseFeatureFile, executeScenario, executeFeature, StepExecutor };
