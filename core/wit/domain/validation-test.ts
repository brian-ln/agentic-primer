#!/usr/bin/env tsx
/**
 * Validation Test for Domain Model
 *
 * Tests that all examples in examples.json validate correctly against:
 * 1. TypeScript types (compile-time checking)
 * 2. Zod validators (runtime validation)
 *
 * This ensures consistency between the JSON Schema, generated TypeScript types,
 * and runtime validators.
 */

import { readFileSync } from 'fs';
import { join } from 'path';
import { DomainValidators } from './domain.validators';
import type * as DomainTypes from './domain.types';

const EXAMPLES_PATH = join(__dirname, 'examples.json');

interface ExamplesFile {
  $schema: string;
  description: string;
  examples: Record<string, any>;
}

interface ValidationResult {
  exampleName: string;
  validatorName: string;
  success: boolean;
  error?: string;
  data?: any;
}

// Mapping from example names to validator names (camelCase from kebab-case)
const EXAMPLE_TO_VALIDATOR_MAP: Record<string, string> = {
  'address_simple': 'address',
  'address_with_namespace': 'address',
  'address_edge_scoped': 'address',
  'node_basic': 'node',
  'edge_simple': 'edge',
  'agent_config': 'agentConfig',
  'task_complete': 'taskConfig',
  'session_config': 'sessionConfig',
  'session_message_user': 'sessionMessage',
  'session_message_assistant': 'sessionMessage',
  'human_config': 'humanConfig',
  'approval_request': 'approvalRequest',
  'notification': 'notification',
  'similarity_search_options': 'similarityOptions',
  'similarity_result': 'similarityResult',
  'convergence_detection': 'convergenceDetection',
  'knowledge_artifact': 'knowledgeArtifact',
  'program_metadata': 'programMetadata',
  'invocation_result': 'invocationResult',
};

function loadExamples(): ExamplesFile {
  const content = readFileSync(EXAMPLES_PATH, 'utf-8');
  return JSON.parse(content);
}

function validateExample(
  exampleName: string,
  example: any,
  validatorName: string
): ValidationResult {
  const validator = (DomainValidators as any)[validatorName];

  if (!validator) {
    return {
      exampleName,
      validatorName,
      success: false,
      error: `Validator '${validatorName}' not found. Available: ${Object.keys(DomainValidators).join(', ')}`,
    };
  }

  const result = validator.safeParse(example);

  if (result.success) {
    return {
      exampleName,
      validatorName,
      success: true,
      data: result.data,
    };
  } else {
    return {
      exampleName,
      validatorName,
      success: false,
      error: formatZodError(result.error),
    };
  }
}

function formatZodError(error: any): string {
  return error.errors
    .map((e: any) => {
      const path = e.path.join('.');
      return `  - ${path || 'root'}: ${e.message}`;
    })
    .join('\n');
}

function printResults(results: ValidationResult[]) {
  const passed = results.filter((r) => r.success);
  const failed = results.filter((r) => !r.success);

  console.log('\n=== Validation Results ===\n');

  if (passed.length > 0) {
    console.log(`✓ Passed: ${passed.length} examples\n`);
    passed.forEach((r) => {
      console.log(`  ✓ ${r.exampleName} (${r.validatorName})`);
    });
  }

  if (failed.length > 0) {
    console.log(`\n✗ Failed: ${failed.length} examples\n`);
    failed.forEach((r) => {
      console.log(`  ✗ ${r.exampleName} (${r.validatorName})`);
      console.log(`    Error:\n${r.error}\n`);
    });
  }

  console.log('\n=== Summary ===');
  console.log(`Total: ${results.length}`);
  console.log(`Passed: ${passed.length}`);
  console.log(`Failed: ${failed.length}`);
  console.log(`Success Rate: ${((passed.length / results.length) * 100).toFixed(1)}%`);

  return failed.length === 0;
}

function main() {
  try {
    console.log('=== Domain Model Validation Test ===');
    console.log('\nLoading examples from:', EXAMPLES_PATH);

    const examplesFile = loadExamples();
    const examples = examplesFile.examples;

    console.log(`Found ${Object.keys(examples).length} examples`);
    console.log(`Available validators: ${Object.keys(DomainValidators).length}`);

    const results: ValidationResult[] = [];

    // Validate each example
    for (const [exampleName, example] of Object.entries(examples)) {
      const validatorName = EXAMPLE_TO_VALIDATOR_MAP[exampleName];

      if (!validatorName) {
        console.log(`\nWarning: No validator mapping for example '${exampleName}'`);
        continue;
      }

      const result = validateExample(exampleName, example, validatorName);
      results.push(result);
    }

    // Print results
    const allPassed = printResults(results);

    // Exit with appropriate code
    process.exit(allPassed ? 0 : 1);
  } catch (error) {
    console.error('\nError during validation:', error);
    process.exit(1);
  }
}

// Type assertions to ensure examples match TypeScript types
// This provides compile-time type checking
function typeCheck() {
  const examples = loadExamples().examples;

  // Address examples
  const address1: DomainTypes.Address = examples.address_simple;
  const address2: DomainTypes.Address = examples.address_with_namespace;
  const address3: DomainTypes.Address = examples.address_edge_scoped;

  // Node example
  const node: DomainTypes.Node = examples.node_basic;

  // Edge example
  const edge: DomainTypes.Edge = examples.edge_simple;

  // Agent config
  const agentConfig: DomainTypes.AgentConfig = examples.agent_config;

  // Task config
  const taskConfig: DomainTypes.TaskConfig = examples.task_complete;

  // Session examples
  const sessionConfig: DomainTypes.SessionConfig = examples.session_config;
  const sessionMsg1: DomainTypes.SessionMessage = examples.session_message_user;
  const sessionMsg2: DomainTypes.SessionMessage = examples.session_message_assistant;

  // Human config
  const humanConfig: DomainTypes.HumanConfig = examples.human_config;

  // Approval and notification
  const approval: DomainTypes.ApprovalRequest = examples.approval_request;
  const notification: DomainTypes.Notification = examples.notification;

  // Similarity search
  const simOptions: DomainTypes.SimilarityOptions = examples.similarity_search_options;
  const simResult: DomainTypes.SimilarityResult = examples.similarity_result;

  // Convergence detection
  const convergence: DomainTypes.ConvergenceDetection = examples.convergence_detection;

  // Knowledge artifact
  const artifact: DomainTypes.KnowledgeArtifact = examples.knowledge_artifact;

  // Program metadata
  const programMeta: DomainTypes.ProgramMetadata = examples.program_metadata;

  // Invocation result
  const invocation: DomainTypes.InvocationResult = examples.invocation_result;

  console.log('Type checking passed!');
}

// Run main validation
main();
