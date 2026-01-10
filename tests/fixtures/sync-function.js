/**
 * Test fixture: Synchronous function
 * Tests that sync functions work too
 */

export default function syncFunction(event, context) {
  const { logger, config } = context;

  logger.info('Sync function called');

  return {
    success: true,
    sync: true,
    configPresent: !!config
  };
}
