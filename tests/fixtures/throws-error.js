/**
 * Test fixture: Function that throws an error
 * Tests error handling
 */

export default async function throwsError(event, context) {
  const { logger } = context;

  logger.info('About to throw error');

  throw new Error('Intentional test error');
}
