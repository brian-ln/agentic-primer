/**
 * Test fixture: Echo function
 * Returns the event data back
 */

export default async function echo(event, context) {
  const { logger } = context;

  logger.info('Echo function called with event:', event.type);

  return {
    success: true,
    echoed: event.data,
    timestamp: new Date().toISOString()
  };
}
