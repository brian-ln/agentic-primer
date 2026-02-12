#!/usr/bin/env bun
/**
 * Example: ETL Workflow
 *
 * Demonstrates Extract → Transform → Load pattern
 * Common data processing workflow
 */

import { buildWorkflow } from '../../src/messaging/actors/workflow-builder.ts';

/**
 * Define the workflow
 */
export const workflow = buildWorkflow('etl-pipeline', 'ETL Data Pipeline')
  .describe('Extract data from sources, transform, and load to warehouse')
  .task('extract-users', {
    title: 'Extract User Data',
    description: 'Pull user records from PostgreSQL',
    priority: 'P1'
  })
  .task('extract-events', {
    title: 'Extract Event Data',
    description: 'Pull event logs from S3',
    priority: 'P1'
  })
  .task('validate', {
    title: 'Validate Extracted Data',
    description: 'Check data quality and schema compliance',
    dependsOn: ['extract-users', 'extract-events'],
    priority: 'P1'
  })
  .task('transform-users', {
    title: 'Transform User Data',
    description: 'Normalize and enrich user records',
    dependsOn: ['validate'],
    priority: 'P1'
  })
  .task('transform-events', {
    title: 'Transform Event Data',
    description: 'Aggregate and denormalize events',
    dependsOn: ['validate'],
    priority: 'P1'
  })
  .task('load', {
    title: 'Load to Warehouse',
    description: 'Write transformed data to BigQuery',
    dependsOn: ['transform-users', 'transform-events'],
    priority: 'P0'
  })
  .task('notify', {
    title: 'Send Completion Notification',
    description: 'Alert stakeholders of successful ETL run',
    dependsOn: ['load'],
    priority: 'P2'
  })
  .build();

/**
 * Visualize the workflow
 */
if (import.meta.main) {
  console.log('ETL Workflow\n');
  console.log(
    buildWorkflow('etl-pipeline', 'ETL Data Pipeline')
      .describe('Extract data from sources, transform, and load to warehouse')
      .task('extract-users', {
        title: 'Extract User Data',
        description: 'Pull user records from PostgreSQL',
        priority: 'P1'
      })
      .task('extract-events', {
        title: 'Extract Event Data',
        description: 'Pull event logs from S3',
        priority: 'P1'
      })
      .task('validate', {
        title: 'Validate Extracted Data',
        description: 'Check data quality and schema compliance',
        dependsOn: ['extract-users', 'extract-events'],
        priority: 'P1'
      })
      .task('transform-users', {
        title: 'Transform User Data',
        description: 'Normalize and enrich user records',
        dependsOn: ['validate'],
        priority: 'P1'
      })
      .task('transform-events', {
        title: 'Transform Event Data',
        description: 'Aggregate and denormalize events',
        dependsOn: ['validate'],
        priority: 'P1'
      })
      .task('load', {
        title: 'Load to Warehouse',
        description: 'Write transformed data to BigQuery',
        dependsOn: ['transform-users', 'transform-events'],
        priority: 'P0'
      })
      .task('notify', {
        title: 'Send Completion Notification',
        description: 'Alert stakeholders of successful ETL run',
        dependsOn: ['load'],
        priority: 'P2'
      })
      .visualize()
  );
}
