/**
 * Echo Function
 *
 * Simple example function that echoes event data back
 * Demonstrates basic code-based function structure
 */

export default async function echo(event, context) {
  // Log the incoming event
  context.logger?.info?.(`Echo function triggered by event: ${event.type}`);

  // Echo the event data back as a new event
  await context.emit({
    type: 'echo.response',
    data: {
      original: event.data,
      echoedAt: new Date().toISOString(),
      message: `Echo: ${JSON.stringify(event.data)}`
    }
  });

  return {
    success: true,
    echoed: event.data
  };
}

// Function metadata (optional, for documentation)
export const metadata = {
  name: 'Echo Function',
  description: 'Echoes event data back as a new event',
  author: 'Event System',
  version: '1.0.0'
};
