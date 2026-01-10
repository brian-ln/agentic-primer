/**
 * Analyze Error Agent Function
 *
 * Uses Claude CLI to analyze error events and suggest fixes
 * Demonstrates agent-based function that calls external AI
 *
 * Agent functions are identified by the .agent.js extension
 * They will be executed using the Claude CLI with the event data as input
 */

// This file serves as metadata and configuration for the agent function
// The actual execution will be handled by the FunctionExecutor actor
// which will invoke: claude <prompt-with-event-data>

export const agentPrompt = `
You are an error analysis agent. Analyze the error event data and provide:

1. Root cause analysis
2. Suggested fix
3. Prevention strategies

Event data will be provided as JSON.
Respond with a structured analysis.
`;

export const metadata = {
  name: 'Error Analysis Agent',
  description: 'Analyzes error events using Claude and suggests fixes',
  author: 'Event System',
  version: '1.0.0',
  agentType: 'analysis',
  model: 'sonnet-4.5'
};
