#!/usr/bin/env bun
/**
 * InferenceActor Usage Example
 *
 * Demonstrates how to use InferenceActor for LLM inference
 * both directly and via the query layer.
 */

import GraphStore from '../src/graph.ts';
import { ProgramManager } from '../src/entities/program.ts';
import { MessageRouter } from '../src/messaging/router.ts';
import { InferenceActor } from '../src/messaging/actors/inference.ts';
import { address } from '../src/messaging/message.ts';

async function main() {
  console.log('=== InferenceActor Usage Examples ===\n');

  // Initialize system
  const store = new GraphStore();
  const programManager = new ProgramManager(store);
  const router = new MessageRouter(store, programManager);

  // Create InferenceActor (will read ANTHROPIC_API_KEY from environment)
  const inference = new InferenceActor('inference', router);

  // Example 1: Simple text generation
  console.log('Example 1: Simple Text Generation');
  console.log('----------------------------------');
  try {
    const response = await inference.ask(
      address('services/inference'),
      'generate',
      {
        prompt: 'Explain what an actor system is in one sentence.',
        maxTokens: 100,
      }
    );

    if (response.success) {
      console.log('Response:', response.payload.content);
      console.log('Tokens:', response.payload.usage);
    } else {
      console.log('Error:', response.error);
    }
  } catch (error: any) {
    console.log('Error:', error.message);
  }
  console.log();

  // Example 2: Using system prompt
  console.log('Example 2: Using System Prompt');
  console.log('-------------------------------');
  try {
    const response = await inference.ask(
      address('services/inference'),
      'generate',
      {
        prompt: 'What is the capital of France?',
        system: 'You are a geography expert. Answer concisely.',
        maxTokens: 50,
      }
    );

    if (response.success) {
      console.log('Response:', response.payload.content);
    } else {
      console.log('Error:', response.error);
    }
  } catch (error: any) {
    console.log('Error:', error.message);
  }
  console.log();

  // Example 3: Template interpolation
  console.log('Example 3: Template Interpolation');
  console.log('----------------------------------');
  try {
    const response = await inference.ask(
      address('services/inference'),
      'generate',
      {
        prompt: 'Explain {{topic}} to a {{level}} student.',
      },
      {
        metadata: {
          topic: 'quantum computing',
          level: 'high school',
        },
      }
    );

    if (response.success) {
      console.log('Response:', response.payload.content);
    } else {
      console.log('Error:', response.error);
    }
  } catch (error: any) {
    console.log('Error:', error.message);
  }
  console.log();

  // Example 4: Different models
  console.log('Example 4: Using Different Models');
  console.log('----------------------------------');
  try {
    const response = await inference.ask(
      address('services/inference'),
      'generate',
      {
        prompt: 'Write a haiku about coding.',
        model: 'claude-haiku-4.5', // Faster, cheaper model
        maxTokens: 100,
      }
    );

    if (response.success) {
      console.log('Response:', response.payload.content);
      console.log('Model used:', response.payload.model);
    } else {
      console.log('Error:', response.error);
    }
  } catch (error: any) {
    console.log('Error:', error.message);
  }
  console.log();

  // Example 5: Tool use
  console.log('Example 5: Tool Use');
  console.log('-------------------');
  try {
    const response = await inference.ask(
      address('services/inference'),
      'generate',
      {
        prompt: 'What is the weather in San Francisco?',
        tools: [
          {
            name: 'get_weather',
            description: 'Get current weather for a location',
            input_schema: {
              type: 'object',
              properties: {
                location: {
                  type: 'string',
                  description: 'City name',
                },
              },
              required: ['location'],
            },
          },
        ],
        maxTokens: 500,
      }
    );

    if (response.success) {
      console.log('Response:', response.payload.content);
      if (response.payload.toolUses) {
        console.log('Tool uses:');
        for (const toolUse of response.payload.toolUses) {
          console.log(`  - ${toolUse.name}(${JSON.stringify(toolUse.input)})`);
        }
      }
    } else {
      console.log('Error:', response.error);
    }
  } catch (error: any) {
    console.log('Error:', error.message);
  }
  console.log();

  // Example 6: Check statistics
  console.log('Example 6: Usage Statistics');
  console.log('---------------------------');
  try {
    const response = await inference.ask(
      address('services/inference'),
      'get-stats',
      {}
    );

    if (response.success) {
      console.log('Total input tokens:', response.payload.totalInputTokens);
      console.log('Total output tokens:', response.payload.totalOutputTokens);
      console.log('Total cost: $' + response.payload.totalCost.toFixed(4));
      console.log('Configured:', response.payload.configured);
    }
  } catch (error: any) {
    console.log('Error:', error.message);
  }
  console.log();

  // Example 7: Query layer integration (conceptual)
  console.log('Example 7: Query Layer Integration (Conceptual)');
  console.log('------------------------------------------------');
  console.log(`
  // In a real query scenario, you could use:

  query()
    .match(pattern('question').where({ status: 'pending' }))
    .forEach(
      send('@(inference)').ask({
        prompt: 'Answer this question: {{question.text}}',
        maxTokens: 500
      })
    )
    .forEach(
      update('question').set({
        answer: '$response.content',
        status: 'answered'
      })
    )
  `);
}

// Run if executed directly
if (import.meta.main) {
  main().catch(console.error);
}
