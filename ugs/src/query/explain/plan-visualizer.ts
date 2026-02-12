#!/usr/bin/env bun
/**
 * Plan Visualizer
 *
 * Creates ASCII tree visualization of query execution plan showing
 * dependencies, parallelism, and data flow.
 */

import type { QueryPlan, PlanStep } from '../types.ts';
import type { TreeNode } from './types.ts';

/**
 * Visualize query plan as ASCII tree
 */
export class PlanVisualizer {
  /**
   * Generate ASCII tree visualization
   */
  visualize(plan: QueryPlan, showCosts = true): string {
    const sections: string[] = [];

    sections.push('EXECUTION TREE');
    sections.push('='.repeat(60));
    sections.push('');

    // Build dependency tree
    const tree = this.buildDependencyTree(plan);

    // Render each root node (steps with no dependencies)
    const roots = tree.filter((node) => node.children.length === 0);
    if (roots.length === 0) {
      // Fallback: if no roots found, render all nodes
      for (const node of tree) {
        sections.push(this.renderNode(node, '', true, showCosts));
      }
    } else {
      for (let i = 0; i < roots.length; i++) {
        const isLast = i === roots.length - 1;
        sections.push(this.renderNode(roots[i], '', isLast, showCosts));
      }
    }

    // Legend
    sections.push('');
    sections.push('LEGEND');
    sections.push('  ├─ Sequential dependency (must wait)');
    sections.push('  ╠═ Parallel branch (can run concurrently)');
    sections.push('  [Q] Query step');
    sections.push('  [T] Traverse step');
    sections.push('  [A] Action step');
    sections.push('  [F] Filter step');
    sections.push('  [G] Aggregate step');

    return sections.join('\n');
  }

  /**
   * Build dependency tree structure
   */
  private buildDependencyTree(plan: QueryPlan): TreeNode[] {
    const stepMap = new Map<string, PlanStep>(
      plan.steps.map((s) => [s.id, s])
    );

    // Build reverse dependency map (step → steps that depend on it)
    const dependents = new Map<string, string[]>();
    for (const step of plan.steps) {
      for (const depId of step.dependencies) {
        if (!dependents.has(depId)) {
          dependents.set(depId, []);
        }
        dependents.get(depId)!.push(step.id);
      }
    }

    // Create nodes in execution order (topological sort)
    const nodes: TreeNode[] = [];
    const visited = new Set<string>();

    const createNode = (stepId: string): TreeNode => {
      if (visited.has(stepId)) {
        // Find existing node
        return nodes.find((n) => n.step.id === stepId)!;
      }

      const step = stepMap.get(stepId)!;
      visited.add(stepId);

      // Create node
      const node: TreeNode = {
        step,
        children: [],
        label: this.formatStepLabel(step),
        details: this.formatStepDetails(step),
      };

      // Add children (steps this depends on)
      for (const depId of step.dependencies) {
        const childNode = createNode(depId);
        node.children.push(childNode);
      }

      nodes.push(node);
      return node;
    };

    // Create nodes for all steps
    for (const step of plan.steps) {
      if (!visited.has(step.id)) {
        createNode(step.id);
      }
    }

    return nodes;
  }

  /**
   * Render a tree node recursively
   */
  private renderNode(
    node: TreeNode,
    prefix: string,
    isLast: boolean,
    showCosts: boolean
  ): string {
    const lines: string[] = [];

    // Node connector
    const connector = isLast ? '└─' : '├─';

    // Parallelism indicator
    const parallelIndicator = node.step.parallelizable ? '╠═' : '├─';

    // Node line
    const costInfo = showCosts
      ? ` (${node.step.cost.latencyMs.toFixed(1)}ms)`
      : '';
    lines.push(`${prefix}${connector} ${node.label}${costInfo}`);

    // Node details (indented)
    const detailPrefix = prefix + (isLast ? '   ' : '│  ');
    for (const detail of node.details) {
      lines.push(`${detailPrefix}${detail}`);
    }

    // Render children
    const childPrefix = prefix + (isLast ? '   ' : '│  ');
    for (let i = 0; i < node.children.length; i++) {
      const childIsLast = i === node.children.length - 1;
      lines.push(
        this.renderNode(node.children[i], childPrefix, childIsLast, showCosts)
      );
    }

    return lines.join('\n');
  }

  /**
   * Format step label
   */
  private formatStepLabel(step: PlanStep): string {
    const typeIcon = this.getTypeIcon(step.type);
    const actorName = this.shortenAddress(step.actor);
    return `${typeIcon} ${step.id} → ${actorName}`;
  }

  /**
   * Format step details
   */
  private formatStepDetails(step: PlanStep): string[] {
    const details: string[] = [];

    // Bindings
    if (step.bindings.length > 0) {
      details.push(`→ produces: ${step.bindings.join(', ')}`);
    }

    // Dependencies
    if (step.dependencies.length > 0) {
      details.push(`← depends: ${step.dependencies.join(', ')}`);
    }

    // Message
    details.push(`✉ ${step.message.pattern} ${step.message.type}`);

    // Cache hint
    const cacheProb = step.cost.cacheHitProb;
    if (cacheProb > 0.7) {
      details.push(`⚡ cache: high (${(cacheProb * 100).toFixed(0)}%)`);
    } else if (cacheProb > 0.3) {
      details.push(`⚡ cache: medium (${(cacheProb * 100).toFixed(0)}%)`);
    } else {
      details.push(`❄ cache: cold (${(cacheProb * 100).toFixed(0)}%)`);
    }

    return details;
  }

  /**
   * Get icon for step type
   */
  private getTypeIcon(type: string): string {
    const icons: Record<string, string> = {
      query: '[Q]',
      traverse: '[T]',
      action: '[A]',
      filter: '[F]',
      aggregate: '[G]',
    };
    return icons[type] || '[?]';
  }

  /**
   * Shorten actor address for display
   */
  private shortenAddress(address: string): string {
    // Remove @() wrapper if present
    const cleaned = address.replace(/^@\(/, '').replace(/\)$/, '');

    // Take last component if path-like
    const parts = cleaned.split('/');
    return parts[parts.length - 1] || cleaned;
  }

  /**
   * Generate execution flow diagram
   */
  visualizeFlow(plan: QueryPlan): string {
    const sections: string[] = [];

    sections.push('EXECUTION FLOW');
    sections.push('='.repeat(60));
    sections.push('');

    // Group steps by execution stage (based on dependencies)
    const stages = this.groupByStage(plan);

    for (let i = 0; i < stages.length; i++) {
      sections.push(`Stage ${i + 1}: ${this.formatStageLatency(stages[i])}`);
      sections.push('');

      const stage = stages[i];
      if (stage.length === 1) {
        // Single step in stage
        sections.push(`  ${this.formatFlowStep(stage[0])}`);
      } else {
        // Multiple parallel steps
        sections.push(`  Parallel execution (${stage.length} steps):`);
        for (const step of stage) {
          sections.push(`    • ${this.formatFlowStep(step)}`);
        }
      }

      // Arrow to next stage
      if (i < stages.length - 1) {
        sections.push('  ↓');
      }

      sections.push('');
    }

    return sections.join('\n');
  }

  /**
   * Group steps by execution stage
   */
  private groupByStage(plan: QueryPlan): PlanStep[][] {
    const stages: PlanStep[][] = [];
    const processed = new Set<string>();

    // Build dependency depth map
    const depthMap = new Map<string, number>();
    const stepMap = new Map(plan.steps.map((s) => [s.id, s]));

    const calculateDepth = (stepId: string): number => {
      if (depthMap.has(stepId)) {
        return depthMap.get(stepId)!;
      }

      const step = stepMap.get(stepId)!;
      if (step.dependencies.length === 0) {
        depthMap.set(stepId, 0);
        return 0;
      }

      const maxDepth = Math.max(
        ...step.dependencies.map((depId) => calculateDepth(depId))
      );
      const depth = maxDepth + 1;
      depthMap.set(stepId, depth);
      return depth;
    };

    // Calculate depths
    for (const step of plan.steps) {
      calculateDepth(step.id);
    }

    // Group by depth
    const maxDepth = Math.max(...Array.from(depthMap.values()));
    for (let depth = 0; depth <= maxDepth; depth++) {
      const stepsAtDepth = plan.steps.filter(
        (s) => depthMap.get(s.id) === depth
      );
      if (stepsAtDepth.length > 0) {
        stages.push(stepsAtDepth);
      }
    }

    return stages;
  }

  /**
   * Format stage latency
   */
  private formatStageLatency(stage: PlanStep[]): string {
    if (stage.length === 1) {
      return `${stage[0].cost.latencyMs.toFixed(1)}ms`;
    }

    // For parallel stages, latency is the max
    const maxLatency = Math.max(...stage.map((s) => s.cost.latencyMs));
    const totalWork = stage.reduce((sum, s) => sum + s.cost.latencyMs, 0);
    return `${maxLatency.toFixed(1)}ms (${totalWork.toFixed(1)}ms total work)`;
  }

  /**
   * Format flow step
   */
  private formatFlowStep(step: PlanStep): string {
    const typeIcon = this.getTypeIcon(step.type);
    const actorName = this.shortenAddress(step.actor);
    return `${typeIcon} ${step.id}: ${step.message.type} → ${actorName} (${step.cost.latencyMs.toFixed(1)}ms)`;
  }
}
