#!/usr/bin/env node

/**
 * Implementation Plan Generator
 *
 * Creates detailed implementation plans from issue requirements
 * and knowledge base context using structured planning approach.
 */

const core = require('@actions/core');

class ImplementationPlanner {
  constructor(requirements, kbContext) {
    this.requirements = requirements;
    this.kbContext = kbContext;
    this.plan = {
      summary: '',
      files: [],
      dependencies: [],
      steps: [],
      testing: [],
      risks: []
    };
  }

  /**
   * Generate complete implementation plan
   */
  generate() {
    console.log('📝 Generating implementation plan...');

    this.analyzeTechnicalStack();
    this.planFileStructure();
    this.identifyDependencies();
    this.createImplementationSteps();
    this.planTesting();
    this.assessRisks();
    this.createSummary();

    console.log('✅ Implementation plan generated');

    return this.plan;
  }

  /**
   * Analyze requirements to determine technical stack
   */
  analyzeTechnicalStack() {
    const reqText = JSON.stringify(this.requirements).toLowerCase();

    // Detect frameworks and technologies
    const detections = {
      framework: this.detectFramework(reqText),
      database: this.detectDatabase(reqText),
      language: this.detectLanguage(reqText),
      apiType: this.detectApiType(reqText)
    };

    this.plan.techStack = detections;

    console.log(`   Tech stack: ${JSON.stringify(detections)}`);
  }

  detectFramework(text) {
    if (text.includes('react')) return 'React';
    if (text.includes('vue')) return 'Vue';
    if (text.includes('angular')) return 'Angular';
    if (text.includes('express')) return 'Express';
    if (text.includes('fastapi')) return 'FastAPI';
    if (text.includes('django')) return 'Django';
    return 'Unknown';
  }

  detectDatabase(text) {
    if (text.includes('postgres') || text.includes('postgresql')) return 'PostgreSQL';
    if (text.includes('mongo')) return 'MongoDB';
    if (text.includes('mysql')) return 'MySQL';
    if (text.includes('redis')) return 'Redis';
    if (text.includes('sqlite')) return 'SQLite';
    return 'None';
  }

  detectLanguage(text) {
    if (text.includes('typescript') || text.includes('ts')) return 'TypeScript';
    if (text.includes('javascript') || text.includes('js')) return 'JavaScript';
    if (text.includes('python')) return 'Python';
    if (text.includes('java')) return 'Java';
    if (text.includes('go') || text.includes('golang')) return 'Go';
    return 'JavaScript'; // Default
  }

  detectApiType(text) {
    if (text.includes('graphql')) return 'GraphQL';
    if (text.includes('grpc')) return 'gRPC';
    if (text.includes('rest') || text.includes('api')) return 'REST';
    return 'REST'; // Default
  }

  /**
   * Plan file structure based on requirements
   */
  planFileStructure() {
    const files = [];
    const reqText = JSON.stringify(this.requirements).toLowerCase();

    // API/Route files
    if (reqText.includes('endpoint') || reqText.includes('api')) {
      files.push({
        path: 'src/routes/feature.routes.js',
        type: 'route',
        purpose: 'Define API endpoints and route handlers'
      });
    }

    // Service layer
    if (reqText.includes('logic') || reqText.includes('service') || files.some(f => f.type === 'route')) {
      files.push({
        path: 'src/services/feature.service.js',
        type: 'service',
        purpose: 'Implement business logic'
      });
    }

    // Data model
    if (reqText.includes('model') || reqText.includes('schema') || reqText.includes('database')) {
      files.push({
        path: 'src/models/Feature.js',
        type: 'model',
        purpose: 'Define data schema and model'
      });
    }

    // Middleware
    if (reqText.includes('auth') || reqText.includes('middleware') || reqText.includes('validation')) {
      files.push({
        path: 'src/middleware/feature.middleware.js',
        type: 'middleware',
        purpose: 'Handle request validation and authentication'
      });
    }

    // Utilities
    if (reqText.includes('util') || reqText.includes('helper')) {
      files.push({
        path: 'src/utils/feature.utils.js',
        type: 'util',
        purpose: 'Helper functions and utilities'
      });
    }

    // Tests
    files.push({
      path: 'tests/feature.test.js',
      type: 'test',
      purpose: 'Unit and integration tests'
    });

    // Documentation
    if (this.requirements.complexity === 'high') {
      files.push({
        path: 'docs/feature.md',
        type: 'documentation',
        purpose: 'Technical documentation and usage guide'
      });
    }

    this.plan.files = files;

    console.log(`   Planned ${files.length} files`);
  }

  /**
   * Identify required dependencies
   */
  identifyDependencies() {
    const deps = [];
    const reqText = JSON.stringify(this.requirements).toLowerCase();

    // Authentication
    if (reqText.includes('jwt') || reqText.includes('token')) {
      deps.push({
        name: 'jsonwebtoken',
        version: '^9.0.0',
        purpose: 'JWT token generation and validation'
      });
    }

    if (reqText.includes('bcrypt') || reqText.includes('password')) {
      deps.push({
        name: 'bcrypt',
        version: '^5.1.0',
        purpose: 'Password hashing'
      });
    }

    // Email
    if (reqText.includes('email')) {
      deps.push({
        name: 'nodemailer',
        version: '^6.9.0',
        purpose: 'Email sending'
      });
    }

    // Validation
    if (reqText.includes('validation') || reqText.includes('validate')) {
      deps.push({
        name: 'joi',
        version: '^17.9.0',
        purpose: 'Request validation'
      });
    }

    // Database drivers
    if (this.plan.techStack.database === 'MongoDB') {
      deps.push({
        name: 'mongoose',
        version: '^7.0.0',
        purpose: 'MongoDB ODM'
      });
    }

    if (this.plan.techStack.database === 'PostgreSQL') {
      deps.push({
        name: 'pg',
        version: '^8.11.0',
        purpose: 'PostgreSQL client'
      });
    }

    // Testing
    deps.push({
      name: 'jest',
      version: '^29.5.0',
      purpose: 'Testing framework',
      devDependency: true
    });

    deps.push({
      name: 'supertest',
      version: '^6.3.0',
      purpose: 'HTTP assertion library',
      devDependency: true
    });

    this.plan.dependencies = deps;

    console.log(`   Identified ${deps.length} dependencies`);
  }

  /**
   * Create step-by-step implementation plan
   */
  createImplementationSteps() {
    const steps = [];

    // Step 1: Setup
    steps.push({
      order: 1,
      title: 'Setup and dependencies',
      tasks: [
        'Install required npm packages',
        'Update package.json',
        'Configure environment variables if needed'
      ]
    });

    // Step 2: Data models
    if (this.plan.files.some(f => f.type === 'model')) {
      steps.push({
        order: 2,
        title: 'Create data models',
        tasks: [
          'Define schema structure',
          'Add validation rules',
          'Create database indexes',
          'Add model methods'
        ]
      });
    }

    // Step 3: Service layer
    if (this.plan.files.some(f => f.type === 'service')) {
      steps.push({
        order: 3,
        title: 'Implement business logic',
        tasks: [
          'Create service class/functions',
          'Implement core functionality',
          'Add error handling',
          'Integrate with data models'
        ]
      });
    }

    // Step 4: API layer
    if (this.plan.files.some(f => f.type === 'route')) {
      steps.push({
        order: 4,
        title: 'Create API endpoints',
        tasks: [
          'Define route handlers',
          'Add request validation',
          'Implement response formatting',
          'Add middleware integration'
        ]
      });
    }

    // Step 5: Testing
    steps.push({
      order: 5,
      title: 'Write tests',
      tasks: [
        'Create test fixtures',
        'Write unit tests for services',
        'Write integration tests for APIs',
        'Add edge case coverage'
      ]
    });

    // Step 6: Documentation
    steps.push({
      order: 6,
      title: 'Documentation',
      tasks: [
        'Add inline code comments',
        'Update API documentation',
        'Add usage examples',
        'Update README if needed'
      ]
    });

    this.plan.steps = steps;

    console.log(`   Created ${steps.length} implementation steps`);
  }

  /**
   * Plan testing approach
   */
  planTesting() {
    const testing = {
      unit: [],
      integration: [],
      e2e: []
    };

    // Unit tests for each service file
    this.plan.files
      .filter(f => f.type === 'service')
      .forEach(file => {
        testing.unit.push({
          target: file.path,
          scenarios: [
            'Happy path with valid inputs',
            'Error handling with invalid inputs',
            'Edge cases and boundary conditions'
          ]
        });
      });

    // Integration tests for APIs
    this.plan.files
      .filter(f => f.type === 'route')
      .forEach(file => {
        testing.integration.push({
          target: file.path,
          scenarios: [
            'Successful API requests',
            'Authentication/authorization failures',
            'Validation errors',
            'Database integration'
          ]
        });
      });

    // E2E tests for critical flows
    if (this.requirements.priority <= 1) {
      testing.e2e.push({
        flow: 'Complete user workflow',
        scenarios: [
          'End-to-end happy path',
          'Error recovery',
          'Performance under load'
        ]
      });
    }

    this.plan.testing = testing;

    console.log(`   Planned testing: ${testing.unit.length} unit, ${testing.integration.length} integration`);
  }

  /**
   * Assess implementation risks
   */
  assessRisks() {
    const risks = [];

    // Complexity risk
    if (this.requirements.complexity === 'high') {
      risks.push({
        level: 'high',
        description: 'High complexity implementation',
        mitigation: 'Break into smaller PRs, increase test coverage'
      });
    }

    // Dependency risk
    if (this.plan.dependencies.length > 5) {
      risks.push({
        level: 'medium',
        description: 'Many new dependencies',
        mitigation: 'Security audit, version pinning'
      });
    }

    // Database migration risk
    if (this.plan.files.some(f => f.type === 'model')) {
      risks.push({
        level: 'medium',
        description: 'Database schema changes',
        mitigation: 'Create migration scripts, backup data'
      });
    }

    // Authentication risk
    const reqText = JSON.stringify(this.requirements).toLowerCase();
    if (reqText.includes('auth') || reqText.includes('security')) {
      risks.push({
        level: 'high',
        description: 'Security-sensitive feature',
        mitigation: 'Security review, penetration testing'
      });
    }

    this.plan.risks = risks;

    console.log(`   Identified ${risks.length} risks`);
  }

  /**
   * Create plan summary
   */
  createSummary() {
    const fileCount = this.plan.files.length;
    const depCount = this.plan.dependencies.filter(d => !d.devDependency).length;
    const testFiles = this.plan.files.filter(f => f.type === 'test').length;

    this.plan.summary = `Implement ${this.requirements.title || 'feature'} with ${fileCount} files, ${depCount} dependencies, and ${testFiles} test suites. Complexity: ${this.requirements.complexity || 'medium'}, Priority: ${this.requirements.priority || 2}.`;

    console.log(`   Summary: ${this.plan.summary}`);
  }

  /**
   * Format plan for output
   */
  format() {
    return {
      summary: this.plan.summary,
      techStack: this.plan.techStack,
      files: this.plan.files,
      dependencies: this.plan.dependencies,
      steps: this.plan.steps,
      testing: this.plan.testing,
      risks: this.plan.risks,
      estimatedHours: this.estimateEffort()
    };
  }

  /**
   * Estimate implementation effort in hours
   */
  estimateEffort() {
    let hours = 0;

    // Base hours per file type
    const fileHours = {
      route: 2,
      service: 3,
      model: 2,
      middleware: 1.5,
      util: 1,
      test: 2,
      documentation: 1
    };

    this.plan.files.forEach(file => {
      hours += fileHours[file.type] || 2;
    });

    // Complexity multiplier
    const complexityMultiplier = {
      low: 1,
      medium: 1.5,
      high: 2.5
    };

    hours *= complexityMultiplier[this.requirements.complexity || 'medium'];

    return Math.ceil(hours);
  }
}

/**
 * Main execution
 */
async function main() {
  const args = process.argv.slice(2);
  let requirements, kbContext;

  for (let i = 0; i < args.length; i += 2) {
    const flag = args[i];
    const value = args[i + 1];

    if (flag === '--requirements') {
      requirements = JSON.parse(value);
    }
    if (flag === '--kb-context') {
      kbContext = JSON.parse(value);
    }
  }

  if (!requirements) {
    console.error('Usage: generate-plan.js --requirements <json> --kb-context <json>');
    process.exit(1);
  }

  const planner = new ImplementationPlanner(requirements, kbContext || []);
  const plan = planner.generate();
  const formatted = planner.format();

  // Output plan
  console.log('\n📋 Implementation Plan:');
  console.log(JSON.stringify(formatted, null, 2));

  // Set GitHub Actions outputs
  core.setOutput('plan', JSON.stringify(formatted));
  core.setOutput('plan_summary', formatted.summary);
  core.setOutput('file_list', formatted.files.map(f => f.path).join('\n'));
  core.setOutput('estimated_hours', formatted.estimatedHours);

  console.log('\n✅ Plan generation complete');
}

// Run
main().catch(error => {
  console.error('❌ Error:', error.message);
  core.setFailed(error.message);
  process.exit(1);
});
