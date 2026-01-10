/**
 * Transform Function
 *
 * Transforms event data according to configured rules
 * Demonstrates data transformation and conditional logic
 */

export default async function transform(event, context) {
  const { data } = event;

  // Example transformation: uppercase all string values
  const transformed = {};
  for (const [key, value] of Object.entries(data)) {
    if (typeof value === 'string') {
      transformed[key] = value.toUpperCase();
    } else if (typeof value === 'number') {
      transformed[key] = value * 2;
    } else {
      transformed[key] = value;
    }
  }

  // Emit transformed data
  await context.emit({
    type: 'data.transformed',
    data: {
      original: data,
      transformed,
      transformedAt: new Date().toISOString()
    }
  });

  return {
    success: true,
    transformedCount: Object.keys(transformed).length
  };
}

export const metadata = {
  name: 'Transform Function',
  description: 'Transforms event data (uppercase strings, double numbers)',
  author: 'Event System',
  version: '1.0.0'
};
