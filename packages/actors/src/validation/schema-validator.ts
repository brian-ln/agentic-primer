#!/usr/bin/env bun
/**
 * Simple JSON Schema validator for introspection protocol
 *
 * Validates message payloads against JSON Schema definitions.
 * Used by Actor base class for acceptance criteria checking.
 */

import type { JSONSchema } from '../introspection.ts';

export interface ValidationError {
  path: string;
  message: string;
}

/**
 * Validate a value against a JSON Schema
 *
 * @param value - Value to validate
 * @param schema - JSON Schema to validate against
 * @returns true if valid, false otherwise
 */
export function validateJSONSchema(value: any, schema: JSONSchema): boolean {
  const errors = validateJSONSchemaErrors(value, schema);
  return errors.length === 0;
}

/**
 * Validate a value against a JSON Schema, returning detailed errors
 *
 * @param value - Value to validate
 * @param schema - JSON Schema to validate against
 * @param path - Current path (for nested validation)
 * @returns Array of validation errors (empty if valid)
 */
export function validateJSONSchemaErrors(
  value: any,
  schema: JSONSchema,
  path: string = '$'
): ValidationError[] {
  const errors: ValidationError[] = [];

  // Type validation
  if (schema.type) {
    const types = Array.isArray(schema.type) ? schema.type : [schema.type];
    const actualType = getJSONType(value);

    if (!types.includes(actualType)) {
      errors.push({
        path,
        message: `Expected type ${types.join(' or ')}, got ${actualType}`,
      });
      // Type mismatch, stop further validation
      return errors;
    }
  }

  // Const validation
  if (schema.const !== undefined) {
    if (value !== schema.const) {
      errors.push({
        path,
        message: `Expected constant value ${JSON.stringify(schema.const)}, got ${JSON.stringify(value)}`,
      });
    }
  }

  // Enum validation
  if (schema.enum) {
    if (!schema.enum.includes(value)) {
      errors.push({
        path,
        message: `Value must be one of: ${schema.enum.map(v => JSON.stringify(v)).join(', ')}`,
      });
    }
  }

  // String validations
  if (typeof value === 'string') {
    if (schema.minLength !== undefined && value.length < schema.minLength) {
      errors.push({
        path,
        message: `String length ${value.length} is less than minimum ${schema.minLength}`,
      });
    }
    if (schema.maxLength !== undefined && value.length > schema.maxLength) {
      errors.push({
        path,
        message: `String length ${value.length} exceeds maximum ${schema.maxLength}`,
      });
    }
    if (schema.pattern) {
      const regex = new RegExp(schema.pattern);
      if (!regex.test(value)) {
        errors.push({
          path,
          message: `String does not match pattern: ${schema.pattern}`,
        });
      }
    }
  }

  // Number validations
  if (typeof value === 'number') {
    if (schema.minimum !== undefined && value < schema.minimum) {
      errors.push({
        path,
        message: `Number ${value} is less than minimum ${schema.minimum}`,
      });
    }
    if (schema.maximum !== undefined && value > schema.maximum) {
      errors.push({
        path,
        message: `Number ${value} exceeds maximum ${schema.maximum}`,
      });
    }
  }

  // Object validations
  if (typeof value === 'object' && value !== null && !Array.isArray(value)) {
    // Required properties
    if (schema.required) {
      for (const prop of schema.required) {
        if (!(prop in value)) {
          errors.push({
            path: `${path}.${prop}`,
            message: `Missing required property: ${prop}`,
          });
        }
      }
    }

    // Min/max properties
    const propCount = Object.keys(value).length;
    if (schema.minProperties !== undefined && propCount < schema.minProperties) {
      errors.push({
        path,
        message: `Object has ${propCount} properties, minimum is ${schema.minProperties}`,
      });
    }
    if (schema.maxProperties !== undefined && propCount > schema.maxProperties) {
      errors.push({
        path,
        message: `Object has ${propCount} properties, maximum is ${schema.maxProperties}`,
      });
    }

    // Property validation
    if (schema.properties) {
      for (const [prop, propSchema] of Object.entries(schema.properties)) {
        if (prop in value) {
          const propErrors = validateJSONSchemaErrors(
            value[prop],
            propSchema,
            `${path}.${prop}`
          );
          errors.push(...propErrors);
        }
      }
    }
  }

  // Array validations
  if (Array.isArray(value)) {
    if (schema.items) {
      for (let i = 0; i < value.length; i++) {
        const itemErrors = validateJSONSchemaErrors(
          value[i],
          schema.items,
          `${path}[${i}]`
        );
        errors.push(...itemErrors);
      }
    }
  }

  return errors;
}

/**
 * Get JSON Schema type name for a value
 */
function getJSONType(value: any): string {
  if (value === null) return 'null';
  if (Array.isArray(value)) return 'array';

  const type = typeof value;
  if (type === 'object') return 'object';
  if (type === 'boolean') return 'boolean';
  if (type === 'number') {
    return Number.isInteger(value) ? 'integer' : 'number';
  }
  if (type === 'string') return 'string';

  return 'unknown';
}

/**
 * Find the closest matching string from a list (for suggestions)
 *
 * Uses simple Levenshtein distance for fuzzy matching.
 *
 * @param target - Target string
 * @param candidates - List of candidate strings
 * @returns Closest match or null if none found
 */
export function findClosestMatch(target: string, candidates: string[]): string | null {
  if (candidates.length === 0) return null;

  let closest = candidates[0];
  let minDistance = levenshteinDistance(target, closest);

  for (const candidate of candidates.slice(1)) {
    const distance = levenshteinDistance(target, candidate);
    if (distance < minDistance) {
      minDistance = distance;
      closest = candidate;
    }
  }

  // Only return if distance is reasonable (< 50% of target length)
  return minDistance < target.length / 2 ? closest : null;
}

/**
 * Calculate Levenshtein distance between two strings
 */
function levenshteinDistance(a: string, b: string): number {
  if (a.length === 0) return b.length;
  if (b.length === 0) return a.length;

  const matrix: number[][] = [];

  // Initialize first row and column
  for (let i = 0; i <= b.length; i++) {
    matrix[i] = [i];
  }
  for (let j = 0; j <= a.length; j++) {
    matrix[0][j] = j;
  }

  // Fill in the rest of the matrix
  for (let i = 1; i <= b.length; i++) {
    for (let j = 1; j <= a.length; j++) {
      if (b.charAt(i - 1) === a.charAt(j - 1)) {
        matrix[i][j] = matrix[i - 1][j - 1];
      } else {
        matrix[i][j] = Math.min(
          matrix[i - 1][j - 1] + 1, // substitution
          matrix[i][j - 1] + 1,     // insertion
          matrix[i - 1][j] + 1      // deletion
        );
      }
    }
  }

  return matrix[b.length][a.length];
}
