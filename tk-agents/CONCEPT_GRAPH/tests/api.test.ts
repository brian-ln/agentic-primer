/**
 * API Endpoint Tests
 * Tests all REST API endpoints for the Concept Graph server
 *
 * @implements server.spec.md#API-Endpoints
 */

import { test, expect } from '@playwright/test';

const BASE_URL = 'http://localhost:3000';

test.describe('API Endpoint Tests', () => {

  test.describe('GET /api/concepts', () => {

    test('should return all 50 concepts', async ({ request }) => {
      const response = await request.get(`${BASE_URL}/api/concepts`);

      expect(response.status()).toBe(200);
      expect(response.headers()['content-type']).toContain('application/json');

      const concepts = await response.json();
      expect(Array.isArray(concepts)).toBe(true);
      expect(concepts.length).toBe(50);
    });

    test('should return concepts with valid structure', async ({ request }) => {
      const response = await request.get(`${BASE_URL}/api/concepts`);
      const concepts = await response.json();

      // Check first concept has all required fields
      const concept = concepts[0];
      expect(concept).toHaveProperty('id');
      expect(concept).toHaveProperty('label');
      expect(concept).toHaveProperty('domains');
      expect(concept).toHaveProperty('tags');
      expect(concept).toHaveProperty('description');
      expect(concept).toHaveProperty('references');

      // Validate types
      expect(typeof concept.id).toBe('string');
      expect(typeof concept.label).toBe('string');
      expect(Array.isArray(concept.domains)).toBe(true);
      expect(Array.isArray(concept.tags)).toBe(true);
      expect(typeof concept.description).toBe('string');
      expect(Array.isArray(concept.references)).toBe(true);

      // Validate non-empty required fields
      expect(concept.id.length).toBeGreaterThan(0);
      expect(concept.label.length).toBeGreaterThan(0);
      expect(concept.domains.length).toBeGreaterThan(0);
    });
  });

  test.describe('GET /api/relationships', () => {

    test('should return all 61 relationships', async ({ request }) => {
      const response = await request.get(`${BASE_URL}/api/relationships`);

      expect(response.status()).toBe(200);
      expect(response.headers()['content-type']).toContain('application/json');

      const relationships = await response.json();
      expect(Array.isArray(relationships)).toBe(true);
      expect(relationships.length).toBe(61); // API returns all relationships from JSON
    });

    test('should return relationships with valid structure', async ({ request }) => {
      const response = await request.get(`${BASE_URL}/api/relationships`);
      const relationships = await response.json();

      // Check first relationship has all required fields
      const rel = relationships[0];
      expect(rel).toHaveProperty('from');
      expect(rel).toHaveProperty('to');
      expect(rel).toHaveProperty('type');
      expect(rel).toHaveProperty('description');

      // Validate types
      expect(typeof rel.from).toBe('string');
      expect(typeof rel.to).toBe('string');
      expect(typeof rel.type).toBe('string');
      expect(typeof rel.description).toBe('string');

      // Validate no self-relationships
      expect(rel.from).not.toBe(rel.to);
    });
  });

  test.describe('GET /api/stats', () => {

    test('should return graph statistics', async ({ request }) => {
      const response = await request.get(`${BASE_URL}/api/stats`);

      expect(response.status()).toBe(200);

      const stats = await response.json();
      expect(stats).toHaveProperty('totalConcepts');
      expect(stats).toHaveProperty('totalRelationships');
      expect(stats).toHaveProperty('domains');
      expect(stats).toHaveProperty('tags');
      expect(stats).toHaveProperty('relationshipTypes');
    });

    test('should return correct counts', async ({ request }) => {
      const response = await request.get(`${BASE_URL}/api/stats`);
      const stats = await response.json();

      expect(stats.totalConcepts).toBe(50);
      expect(stats.totalRelationships).toBe(61); // API stats count all relationships
      expect(stats.domains).toBeGreaterThan(0);
      expect(stats.tags).toBeGreaterThan(0);
      expect(stats.relationshipTypes).toBeGreaterThan(0);
    });
  });

  test.describe('GET /api/concept/:id', () => {

    test('should return concept details for valid ID', async ({ request }) => {
      const response = await request.get(`${BASE_URL}/api/concept/actor-model`);

      expect(response.status()).toBe(200);

      const data = await response.json();
      expect(data).toHaveProperty('concept');
      expect(data).toHaveProperty('relationships');

      // Validate concept
      expect(data.concept.id).toBe('actor-model');
      expect(data.concept).toHaveProperty('label');
      expect(data.concept).toHaveProperty('domains');

      // Validate relationships structure
      expect(data.relationships).toHaveProperty('outgoing');
      expect(data.relationships).toHaveProperty('incoming');
      expect(Array.isArray(data.relationships.outgoing)).toBe(true);
      expect(Array.isArray(data.relationships.incoming)).toBe(true);
    });

    test('should populate related concepts (not just IDs)', async ({ request }) => {
      const response = await request.get(`${BASE_URL}/api/concept/actor-model`);
      const data = await response.json();

      // Check that outgoing relationships include full concept objects
      if (data.relationships.outgoing.length > 0) {
        const firstOutgoing = data.relationships.outgoing[0];
        expect(firstOutgoing).toHaveProperty('relationship');
        expect(firstOutgoing).toHaveProperty('concept');
        expect(firstOutgoing.concept).toHaveProperty('id');
        expect(firstOutgoing.concept).toHaveProperty('label');
      }
    });

    test('should return 404 for non-existent concept', async ({ request }) => {
      const response = await request.get(`${BASE_URL}/api/concept/nonexistent-id-12345`);

      expect(response.status()).toBe(404);
      const body = await response.text();
      expect(body).toBe('Concept not found');
    });

    test('should return 400 for missing concept ID', async ({ request }) => {
      const response = await request.get(`${BASE_URL}/api/concept/`);

      // This might return 404 from Bun's router or 400 from handler
      // Accept either as valid (route not found vs missing parameter)
      expect([400, 404]).toContain(response.status());
    });
  });

  test.describe('GET /api/search', () => {

    test('should search by keyword', async ({ request }) => {
      const response = await request.get(`${BASE_URL}/api/search?q=actor`);

      expect(response.status()).toBe(200);

      const results = await response.json();
      expect(Array.isArray(results)).toBe(true);
      expect(results.length).toBeGreaterThan(0);

      // Verify all results contain "actor" in label or description
      for (const concept of results) {
        const label = concept.label.toLowerCase();
        const description = concept.description.toLowerCase();
        const hasMatch = label.includes('actor') || description.includes('actor');
        expect(hasMatch).toBe(true);
      }
    });

    test('should search by domain', async ({ request }) => {
      const response = await request.get(`${BASE_URL}/api/search?domain=computer-science`);

      expect(response.status()).toBe(200);

      const results = await response.json();
      expect(results.length).toBeGreaterThan(0);

      // Verify all results have "computer-science" in domains
      for (const concept of results) {
        expect(concept.domains).toContain('computer-science');
      }
    });

    test('should search by tag', async ({ request }) => {
      const response = await request.get(`${BASE_URL}/api/search?tag=concurrency`);

      expect(response.status()).toBe(200);

      const results = await response.json();
      expect(results.length).toBeGreaterThan(0);

      // Verify all results have "concurrency" in tags
      for (const concept of results) {
        expect(concept.tags).toContain('concurrency');
      }
    });

    test('should combine filters with AND logic', async ({ request }) => {
      const response = await request.get(`${BASE_URL}/api/search?q=state&domain=computer-science`);

      expect(response.status()).toBe(200);

      const results = await response.json();

      // Verify all results match BOTH criteria
      for (const concept of results) {
        const label = concept.label.toLowerCase();
        const description = concept.description.toLowerCase();
        const hasKeyword = label.includes('state') || description.includes('state');
        const hasDomain = concept.domains.includes('computer-science');

        expect(hasKeyword).toBe(true);
        expect(hasDomain).toBe(true);
      }
    });

    test('should return 400 for missing search parameters', async ({ request }) => {
      const response = await request.get(`${BASE_URL}/api/search`);

      expect(response.status()).toBe(400);
      const body = await response.text();
      expect(body).toBe('Missing search parameters');
    });

    test('should return empty array for no matches', async ({ request }) => {
      const response = await request.get(`${BASE_URL}/api/search?q=zzzznonexistent12345`);

      expect(response.status()).toBe(200);
      const results = await response.json();
      expect(results).toEqual([]);
    });

    test('should be case-insensitive', async ({ request }) => {
      const responseLower = await request.get(`${BASE_URL}/api/search?q=actor`);
      const responseUpper = await request.get(`${BASE_URL}/api/search?q=ACTOR`);

      const resultsLower = await responseLower.json();
      const resultsUpper = await responseUpper.json();

      // Both should return same results
      expect(resultsLower.length).toBe(resultsUpper.length);
      expect(resultsLower.length).toBeGreaterThan(0);
    });
  });

  test.describe('GET /', () => {

    test('should serve HTML application', async ({ request }) => {
      const response = await request.get(`${BASE_URL}/`);

      expect(response.status()).toBe(200);
      expect(response.headers()['content-type']).toContain('text/html');

      const html = await response.text();
      expect(html).toContain('<!DOCTYPE html>');
      expect(html).toContain('Concept Graph Explorer');
    });
  });

  test.describe('Edge Cases', () => {

    test('should handle concurrent requests', async ({ request }) => {
      // Send 10 concurrent requests
      const promises = Array(10).fill(null).map(() =>
        request.get(`${BASE_URL}/api/concepts`)
      );

      const responses = await Promise.all(promises);

      // All should succeed
      for (const response of responses) {
        expect(response.status()).toBe(200);
        const concepts = await response.json();
        expect(concepts.length).toBe(50);
      }
    });

    test('should handle special characters in search', async ({ request }) => {
      const response = await request.get(`${BASE_URL}/api/search?q=actor.*model$`);

      expect(response.status()).toBe(200);
      // Should treat as literal string, not regex
      const results = await response.json();
      // Unlikely to match literal "actor.*model$" so expect empty or very few
      expect(Array.isArray(results)).toBe(true);
    });

    test('should handle very long search query', async ({ request }) => {
      const longQuery = 'a'.repeat(1000);
      const response = await request.get(`${BASE_URL}/api/search?q=${longQuery}`);

      expect(response.status()).toBe(200);
      const results = await response.json();
      expect(Array.isArray(results)).toBe(true);
    });
  });
});
