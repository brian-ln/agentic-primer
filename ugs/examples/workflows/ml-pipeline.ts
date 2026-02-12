#!/usr/bin/env bun
/**
 * Example: ML Training Pipeline
 *
 * Demonstrates ML workflow: data prep → train → evaluate → deploy
 * Includes parallel training of multiple models
 */

import { buildWorkflow, diamond } from '../../src/messaging/actors/workflow-builder.ts';

/**
 * Define the workflow with parallel model training
 */
export const workflow = buildWorkflow('ml-pipeline', 'ML Training Pipeline')
  .describe('Train and deploy ML models with cross-validation')
  .task('preprocess', {
    title: 'Preprocess Data',
    description: 'Clean, normalize, and split dataset',
    priority: 'P0'
  })
  .task('feature-engineering', {
    title: 'Feature Engineering',
    description: 'Generate features from preprocessed data',
    dependsOn: ['preprocess'],
    priority: 'P0'
  })
  .task('train-xgboost', {
    title: 'Train XGBoost Model',
    description: 'Train gradient boosting model',
    dependsOn: ['feature-engineering'],
    priority: 'P1'
  })
  .task('train-neural-net', {
    title: 'Train Neural Network',
    description: 'Train deep learning model',
    dependsOn: ['feature-engineering'],
    priority: 'P1'
  })
  .task('train-random-forest', {
    title: 'Train Random Forest',
    description: 'Train ensemble model',
    dependsOn: ['feature-engineering'],
    priority: 'P1'
  })
  .task('evaluate', {
    title: 'Evaluate Models',
    description: 'Compare models on validation set',
    dependsOn: ['train-xgboost', 'train-neural-net', 'train-random-forest'],
    priority: 'P0'
  })
  .task('select-best', {
    title: 'Select Best Model',
    description: 'Choose model with highest accuracy',
    dependsOn: ['evaluate'],
    priority: 'P0'
  })
  .task('deploy', {
    title: 'Deploy Model',
    description: 'Deploy selected model to production',
    dependsOn: ['select-best'],
    priority: 'P0'
  })
  .build();

/**
 * Alternative: Simple diamond pattern for A/B model comparison
 */
export const abTestWorkflow = diamond(
  'ml-ab-test',
  'ML A/B Test Pipeline',
  { id: 'preprocess', title: 'Preprocess Data' },
  { id: 'train-baseline', title: 'Train Baseline Model' },
  { id: 'train-experimental', title: 'Train Experimental Model' },
  { id: 'compare', title: 'Compare Models and Deploy Best' }
);

/**
 * Visualize the workflows
 */
if (import.meta.main) {
  console.log('ML Training Pipeline (Parallel Training)\n');
  console.log(
    buildWorkflow('ml-pipeline', 'ML Training Pipeline')
      .describe('Train and deploy ML models with cross-validation')
      .task('preprocess', {
        title: 'Preprocess Data',
        description: 'Clean, normalize, and split dataset',
        priority: 'P0'
      })
      .task('feature-engineering', {
        title: 'Feature Engineering',
        description: 'Generate features from preprocessed data',
        dependsOn: ['preprocess'],
        priority: 'P0'
      })
      .task('train-xgboost', {
        title: 'Train XGBoost Model',
        description: 'Train gradient boosting model',
        dependsOn: ['feature-engineering'],
        priority: 'P1'
      })
      .task('train-neural-net', {
        title: 'Train Neural Network',
        description: 'Train deep learning model',
        dependsOn: ['feature-engineering'],
        priority: 'P1'
      })
      .task('train-random-forest', {
        title: 'Train Random Forest',
        description: 'Train ensemble model',
        dependsOn: ['feature-engineering'],
        priority: 'P1'
      })
      .task('evaluate', {
        title: 'Evaluate Models',
        description: 'Compare models on validation set',
        dependsOn: ['train-xgboost', 'train-neural-net', 'train-random-forest'],
        priority: 'P0'
      })
      .task('select-best', {
        title: 'Select Best Model',
        description: 'Choose model with highest accuracy',
        dependsOn: ['evaluate'],
        priority: 'P0'
      })
      .task('deploy', {
        title: 'Deploy Model',
        description: 'Deploy selected model to production',
        dependsOn: ['select-best'],
        priority: 'P0'
      })
      .visualize()
  );

  console.log('\n\nML A/B Test Pipeline (Diamond Pattern)\n');
  const builder = buildWorkflow('ml-ab-test', 'ML A/B Test Pipeline')
    .task('preprocess', { title: 'Preprocess Data' })
    .task('train-baseline', { title: 'Train Baseline Model', dependsOn: ['preprocess'] })
    .task('train-experimental', { title: 'Train Experimental Model', dependsOn: ['preprocess'] })
    .task('compare', {
      title: 'Compare Models and Deploy Best',
      dependsOn: ['train-baseline', 'train-experimental']
    });
  console.log(builder.visualize());
}
