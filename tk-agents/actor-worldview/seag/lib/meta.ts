/**
 * SEAG Metadata Decorators
 * These tie the Implementation (Code) back to the Design (Model).
 */

export function Actor(name: string) {
  return (constructor: Function) => {
    (constructor as any).modelName = name; // Statically assign for easy lookup
  };
}

export function Handler(messageType: string) {
  return function (target: any, propertyKey: string, descriptor: PropertyDescriptor) {
    // Metadata for consistency checking
  };
}

export function Implements(...protocols: string[]) {
  return function <T extends { new (...args: any[]): {} }>(constructor: T) {
  };
}

/**
 * For marking classes that don't have a direct model but are necessary
 * for implementation (e.g., test mocks, adapters).
 */
export function NativeAdapter(reason: string) {
  return function <T extends { new (...args: any[]): {} }>(constructor: T) {
  };
}
