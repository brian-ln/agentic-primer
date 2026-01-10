/**
 * Test fixture: Function that emits new events
 * Tests that context.emit works correctly
 */

export default async function emitEvent(event, context) {
  const { emit, logger } = context;

  logger.info('Emitting child event');

  // Emit a new event
  await emit({
    type: 'test.child',
    data: {
      parent: event.type,
      message: 'Child event from function'
    }
  });

  return {
    success: true,
    emittedEvent: true
  };
}
