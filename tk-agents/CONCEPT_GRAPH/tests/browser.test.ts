/**
 * Browser Interaction Tests
 * Tests frontend graph visualization and user interactions
 *
 * @implements server.spec.md#Browser-Interaction-Fixtures
 */

import { test, expect } from '@playwright/test';

test.describe('Browser Interaction Tests', () => {

  test.describe('Page Load and Initial Render', () => {

    test('should load page and render graph', async ({ page }) => {
      await page.goto('/');

      // Verify header
      const header = page.locator('h1');
      await expect(header).toContainText('Concept Graph Explorer');

      // Verify subtitle
      const subtitle = page.locator('.subtitle');
      await expect(subtitle).toContainText('Interactive visualization');

      // Wait for data to load and stats to update
      await page.waitForFunction(() => {
        const stats = document.getElementById('conceptCount');
        return stats && stats.textContent !== 'Loading...';
      });

      // Verify stats display correct counts
      const stats = page.locator('#conceptCount');
      await expect(stats).toContainText('50 concepts');
      await expect(stats).toContainText('61 relationships');
    });

    test('should render SVG graph with nodes and edges', async ({ page }) => {
      await page.goto('/');

      // Wait for graph to render
      await page.waitForSelector('svg');
      await page.waitForSelector('.node', { timeout: 5000 });

      // Count nodes (circles)
      const nodes = page.locator('.node circle');
      const nodeCount = await nodes.count();
      expect(nodeCount).toBe(50);

      // Count edges (paths)
      const edges = page.locator('.links path');
      const edgeCount = await edges.count();
      expect(edgeCount).toBe(61);
    });

    test('should display legend with domains', async ({ page }) => {
      await page.goto('/');

      // Wait for legend to render
      await page.waitForSelector('.legend');

      const legendTitle = page.locator('.legend h3');
      await expect(legendTitle).toContainText('Domains');

      // Verify legend has items
      const legendItems = page.locator('.legend-item');
      const itemCount = await legendItems.count();
      expect(itemCount).toBeGreaterThan(10); // At least 10 unique domains
    });
  });

  test.describe('Node Selection and Details', () => {

    test('should show detail panel when clicking a node', async ({ page }) => {
      await page.goto('/');

      // Wait for graph to render
      await page.waitForSelector('.node', { timeout: 5000 });

      // Initially, detail panel should show empty state
      const emptyState = page.locator('.detail-empty');
      await expect(emptyState).toBeVisible();

      // Click the first node
      const firstNode = page.locator('.node').first();
      await firstNode.click();

      // Wait for detail panel to populate
      await page.waitForSelector('#detailContent[style*="block"]', { timeout: 3000 });

      // Detail content should be visible
      const detailContent = page.locator('#detailContent');
      await expect(detailContent).toBeVisible();

      // Empty state should be hidden
      await expect(emptyState).toBeHidden();

      // Verify detail panel has concept information
      const conceptHeading = page.locator('#detailBody h2');
      await expect(conceptHeading).toBeVisible();
      const headingText = await conceptHeading.textContent();
      expect(headingText?.length).toBeGreaterThan(0);
    });

    test('should display concept details correctly', async ({ page }) => {
      await page.goto('/');
      await page.waitForSelector('.node', { timeout: 5000 });

      // Click first node
      await page.locator('.node').first().click();
      await page.waitForSelector('#detailContent[style*="block"]');

      // Check for sections
      const sections = page.locator('.section');
      const sectionCount = await sections.count();
      expect(sectionCount).toBeGreaterThan(0);

      // Verify section titles exist
      const sectionTitles = page.locator('.section-title');
      const titleCount = await sectionTitles.count();
      expect(titleCount).toBeGreaterThan(0);

      // Check for domains section
      const domains = page.locator('.domains .domain');
      const domainCount = await domains.count();
      expect(domainCount).toBeGreaterThan(0);

      // Check for tags section
      const tags = page.locator('.tags .tag');
      const tagCount = await tags.count();
      expect(tagCount).toBeGreaterThan(0);

      // Check for description
      const description = page.locator('.description');
      await expect(description).toBeVisible();
      const descText = await description.textContent();
      expect(descText?.length).toBeGreaterThan(0);
    });

    test('should highlight selected node', async ({ page }) => {
      await page.goto('/');
      await page.waitForSelector('.node', { timeout: 5000 });

      // Click first node
      const firstNode = page.locator('.node').first();
      await firstNode.click();
      await page.waitForTimeout(500); // Wait for selection animation

      // Node should have "selected" class
      const hasSelected = await firstNode.evaluate((node) =>
        node.classList.contains('selected')
      );
      expect(hasSelected).toBe(true);
    });

    test('should highlight connected edges', async ({ page }) => {
      await page.goto('/');
      await page.waitForSelector('.node', { timeout: 5000 });

      // Click first node
      await page.locator('.node').first().click();
      await page.waitForTimeout(500);

      // Some edges should be highlighted
      const highlightedEdges = page.locator('.link.highlighted');
      const count = await highlightedEdges.count();
      expect(count).toBeGreaterThan(0);
    });
  });

  test.describe('Navigation via Relationships', () => {

    test('should navigate to related concept when clicking relationship', async ({ page }) => {
      await page.goto('/');
      await page.waitForSelector('.node', { timeout: 5000 });

      // Click first node
      await page.locator('.node').first().click();
      await page.waitForSelector('#detailContent[style*="block"]');

      // Get initial concept name
      const initialConcept = await page.locator('#detailBody h2').textContent();

      // Wait for relationships to load
      await page.waitForSelector('.relationship', { timeout: 3000 });

      // Click first relationship card
      const firstRelationship = page.locator('.relationship').first();
      await firstRelationship.click();
      await page.waitForTimeout(500);

      // Get new concept name
      const newConcept = await page.locator('#detailBody h2').textContent();

      // Should have navigated to different concept
      expect(newConcept).not.toBe(initialConcept);
    });
  });

  test.describe('Search and Filtering', () => {

    test('should filter nodes when typing in search box', async ({ page }) => {
      await page.goto('/');
      await page.waitForSelector('.node', { timeout: 5000 });

      // Type in search box
      const searchInput = page.locator('#searchInput');
      await searchInput.fill('actor');
      await page.waitForTimeout(500); // Wait for filter to apply

      // Some nodes should be dimmed
      const dimmedNodes = page.locator('.node.dimmed');
      const dimmedCount = await dimmedNodes.count();
      expect(dimmedCount).toBeGreaterThan(0);

      // Some nodes should NOT be dimmed (matching "actor")
      const visibleNodes = page.locator('.node:not(.dimmed)');
      const visibleCount = await visibleNodes.count();
      expect(visibleCount).toBeGreaterThan(0);
      expect(visibleCount).toBeLessThan(50); // Not all nodes match
    });

    test('should clear search and restore all nodes', async ({ page }) => {
      await page.goto('/');
      await page.waitForSelector('.node', { timeout: 5000 });

      // Apply search filter
      const searchInput = page.locator('#searchInput');
      await searchInput.fill('actor');
      await page.waitForTimeout(500);

      // Verify some nodes are dimmed
      const dimmedBefore = await page.locator('.node.dimmed').count();
      expect(dimmedBefore).toBeGreaterThan(0);

      // Click clear button
      const clearButton = page.locator('#clearSearch');
      await clearButton.click();
      await page.waitForTimeout(300);

      // Verify search input is cleared
      const inputValue = await searchInput.inputValue();
      expect(inputValue).toBe('');

      // Verify no nodes are dimmed
      const dimmedAfter = await page.locator('.node.dimmed').count();
      expect(dimmedAfter).toBe(0);
    });
  });

  test.describe('Detail Panel Controls', () => {

    test('should close detail panel when clicking close button', async ({ page }) => {
      await page.goto('/');
      await page.waitForSelector('.node', { timeout: 5000 });

      // Open detail panel
      await page.locator('.node').first().click();
      await page.waitForSelector('#detailContent[style*="block"]');

      // Click close button
      const closeButton = page.locator('#closeDetail');
      await closeButton.click();
      await page.waitForTimeout(300);

      // Detail panel should be hidden
      const detailContent = page.locator('#detailContent');
      await expect(detailContent).toBeHidden();

      // Empty state should be visible
      const emptyState = page.locator('.detail-empty');
      await expect(emptyState).toBeVisible();

      // Node should not have selected class
      const selectedNodes = page.locator('.node.selected');
      const count = await selectedNodes.count();
      expect(count).toBe(0);
    });

    test('should close detail panel when clicking background', async ({ page }) => {
      await page.goto('/');
      await page.waitForSelector('.node', { timeout: 5000 });

      // Open detail panel
      await page.locator('.node').first().click();
      await page.waitForSelector('#detailContent[style*="block"]');

      // Click SVG background (empty space)
      const svg = page.locator('svg');
      await svg.click({ position: { x: 10, y: 10 } }); // Top-left corner
      await page.waitForTimeout(300);

      // Detail panel should be hidden
      const detailContent = page.locator('#detailContent');
      await expect(detailContent).toBeHidden();
    });
  });

  test.describe('Graph Interactions', () => {

    test('should allow dragging nodes', async ({ page }) => {
      await page.goto('/');
      await page.waitForSelector('.node', { timeout: 5000 });

      // Get first node
      const firstNode = page.locator('.node circle').first();

      // Get initial position
      const initialBox = await firstNode.boundingBox();
      expect(initialBox).not.toBeNull();

      // Drag node
      await firstNode.hover();
      await page.mouse.down();
      await page.mouse.move(
        initialBox!.x + 100,
        initialBox!.y + 100,
        { steps: 10 }
      );
      await page.mouse.up();
      await page.waitForTimeout(300);

      // Get new position
      const newBox = await firstNode.boundingBox();
      expect(newBox).not.toBeNull();

      // Position should have changed (allowing for some tolerance)
      const moved = Math.abs(newBox!.x - initialBox!.x) > 50 ||
                    Math.abs(newBox!.y - initialBox!.y) > 50;
      expect(moved).toBe(true);
    });

    test('should support zoom via mouse wheel', async ({ page }) => {
      await page.goto('/');
      await page.waitForSelector('svg', { timeout: 5000 });

      const svg = page.locator('svg');
      const graphGroup = page.locator('svg > g').first();

      // Get initial transform
      const initialTransform = await graphGroup.getAttribute('transform');

      // Zoom in (wheel up)
      await svg.hover();
      await page.mouse.wheel(0, -100); // Negative delta = zoom in
      await page.waitForTimeout(300);

      // Get new transform
      const newTransform = await graphGroup.getAttribute('transform');

      // Transform should have changed
      expect(newTransform).not.toBe(initialTransform);
    });
  });

  test.describe('Responsive Behavior', () => {

    test('should handle window resize', async ({ page }) => {
      await page.goto('/');
      await page.waitForSelector('svg', { timeout: 5000 });

      // Get initial SVG dimensions
      const svg = page.locator('svg');
      const initialWidth = await svg.getAttribute('width');

      // Resize viewport
      await page.setViewportSize({ width: 800, height: 600 });
      await page.waitForTimeout(500);

      // Get new dimensions
      const newWidth = await svg.getAttribute('width');

      // Width should have changed
      expect(newWidth).not.toBe(initialWidth);
    });
  });

  test.describe('No Console Errors', () => {

    test('should load without JavaScript errors', async ({ page }) => {
      const errors: string[] = [];

      page.on('console', (msg) => {
        if (msg.type() === 'error') {
          errors.push(msg.text());
        }
      });

      page.on('pageerror', (error) => {
        errors.push(error.message);
      });

      await page.goto('/');
      await page.waitForSelector('.node', { timeout: 5000 });

      // Give it a moment to settle
      await page.waitForTimeout(1000);

      // Should have no errors
      expect(errors).toEqual([]);
    });
  });
});
