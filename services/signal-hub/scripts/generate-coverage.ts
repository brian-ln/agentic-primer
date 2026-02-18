#!/usr/bin/env bun

/**
 * Spec Coverage Report Generator
 *
 * Extracts testable requirements from spec files and generates
 * a coverage report showing which requirements are tested.
 */

import { readFileSync, writeFileSync, readdirSync } from 'fs';
import { join } from 'path';
import { glob } from 'glob';

const SPEC_DIR = 'spec';
const TEST_DIRS = [
  'src/handlers/__tests__',
  'src/validation/__tests__',
  '../../tests/integration/signal-hub'
];
const OUTPUT_FILE = 'SPEC_COVERAGE.md';
const REQUIREMENTS_FILE = 'spec-requirements.json';

interface Requirement {
  id: string;
  domain: string;
  requirement: string;
  spec: string;
  priority: string;
  tested: boolean;
  testFiles: string[];
  lineNumbers: number[];
}

interface MessageType {
  name: string;
  tested: boolean;
  testFiles: string[];
  lineNumbers: number[];
}

interface ErrorCondition {
  name: string;
  code?: string;
  tested: boolean;
  testFiles: string[];
}

interface StateTransition {
  from: string;
  event: string;
  to: string;
  tested: boolean;
  testFiles: string[];
}

interface CoverageReport {
  generatedDate: string;
  requirements: Requirement[];
  messageTypes: MessageType[];
  errorConditions: ErrorCondition[];
  stateTransitions: StateTransition[];
}

/**
 * Extract testable requirements from spec markdown files
 */
function extractRequirements(): Requirement[] {
  const requirements: Requirement[] = [];
  const domains = ['connection', 'registration', 'messaging', 'pubsub'];

  for (const domain of domains) {
    const specFile = join(SPEC_DIR, domain, `${domain.toUpperCase()}.spec.md`);

    try {
      const content = readFileSync(specFile, 'utf-8');
      const lines = content.split('\n');

      // Extract requirements from spec
      // Look for MUST, SHOULD, SHALL statements and numbered requirements
      for (let i = 0; i < lines.length; i++) {
        const line = lines[i];

        // Match MUST/SHOULD/SHALL statements
        if (/\b(MUST|SHOULD|SHALL)\b/.test(line)) {
          requirements.push({
            id: `${domain}-req-${requirements.filter(r => r.domain === domain).length + 1}`,
            domain,
            requirement: line.trim(),
            spec: `${domain}/${domain.toUpperCase()}.spec.md#L${i + 1}`,
            priority: determinePriority(line),
            tested: false,
            testFiles: [],
            lineNumbers: []
          });
        }

        // Match section headers describing requirements
        if (/^###?\s+/.test(line) && !line.includes('Overview') && !line.includes('Machine-Readable')) {
          const requirement = line.replace(/^###?\s+/, '').trim();
          if (requirement.length > 0 && requirement.length < 100) {
            requirements.push({
              id: `${domain}-req-${requirements.filter(r => r.domain === domain).length + 1}`,
              domain,
              requirement,
              spec: `${domain}/${domain.toUpperCase()}.spec.md#L${i + 1}`,
              priority: 'P1',
              tested: false,
              testFiles: [],
              lineNumbers: []
            });
          }
        }
      }
    } catch (error) {
      console.error(`Error reading spec file ${specFile}:`, error);
    }
  }

  return requirements;
}

/**
 * Determine priority from requirement text
 */
function determinePriority(text: string): string {
  if (text.includes('MUST') || text.includes('SHALL')) return 'P0';
  if (text.includes('SHOULD')) return 'P1';
  return 'P2';
}

/**
 * Extract message types from specs
 */
function extractMessageTypes(): MessageType[] {
  const messageTypes: MessageType[] = [];

  // Known message types from specs
  const types = [
    'hub:connect', 'hub:connected', 'hub:disconnect', 'hub:disconnect_ack',
    'hub:heartbeat', 'hub:heartbeat_ack', 'hub:error',
    'hub:register', 'hub:registered', 'hub:renew', 'hub:renewed', 'hub:unregister', 'hub:unregistered',
    'hub:discover', 'hub:discovery_result',
    'hub:send', 'hub:delivery_ack', 'hub:broadcast',
    'hub:subscribe', 'hub:subscribed', 'hub:unsubscribe', 'hub:unsubscribed', 'hub:publish',
    'hub:pause', 'hub:paused', 'hub:resume', 'hub:resumed'
  ];

  for (const type of types) {
    messageTypes.push({
      name: type,
      tested: false,
      testFiles: [],
      lineNumbers: []
    });
  }

  return messageTypes;
}

/**
 * Extract error conditions from CONNECTION spec
 */
function extractErrorConditions(): ErrorCondition[] {
  return [
    { name: 'Version mismatch', code: 'version_mismatch', tested: false, testFiles: [] },
    { name: 'Invalid JWT', code: 'unauthorized', tested: false, testFiles: [] },
    { name: 'Heartbeat timeout', tested: false, testFiles: [] },
    { name: 'Unknown actor', code: 'unknown_actor', tested: false, testFiles: [] },
    { name: 'Message too large', code: 'message_too_large', tested: false, testFiles: [] },
    { name: 'Rate limited', code: 'rate_limited', tested: false, testFiles: [] },
    { name: 'Invalid message format', code: 'invalid_message', tested: false, testFiles: [] },
    { name: 'Missing required fields', tested: false, testFiles: [] },
  ];
}

/**
 * Extract state transitions from CONNECTION spec
 */
function extractStateTransitions(): StateTransition[] {
  return [
    { from: 'connecting', event: 'hub:connect (valid)', to: 'connected', tested: false, testFiles: [] },
    { from: 'connecting', event: 'hub:connect (invalid)', to: 'connecting', tested: false, testFiles: [] },
    { from: 'connected', event: 'hub:disconnect', to: 'disconnecting', tested: false, testFiles: [] },
    { from: 'disconnecting', event: 'cleanup complete', to: 'disconnected', tested: false, testFiles: [] },
    { from: 'connected', event: 'WebSocket close', to: 'disconnected', tested: false, testFiles: [] },
    { from: 'connected', event: 'heartbeat timeout', to: 'disconnected', tested: false, testFiles: [] },
  ];
}

/**
 * Find test coverage for requirements, message types, errors, and state transitions
 */
async function findTestCoverage(
  requirements: Requirement[],
  messageTypes: MessageType[],
  errorConditions: ErrorCondition[],
  stateTransitions: StateTransition[]
): Promise<void> {
  const testFiles = await glob(TEST_DIRS.map(dir => `${dir}/**/*.test.ts`));

  for (const testFile of testFiles) {
    try {
      const content = readFileSync(testFile, 'utf-8');
      const lines = content.split('\n');

      for (let i = 0; i < lines.length; i++) {
        const line = lines[i];

        // Look for @spec: and @requirement: annotations
        if (line.includes('@spec:')) {
          const specMatch = line.match(/\/\/\s*@spec:\s*(.+)/);
          if (specMatch) {
            const specRef = specMatch[1].trim();
            // Extract line numbers from spec reference (e.g., "#L38-L53" or "#L109-L128")
            const lineMatch = specRef.match(/#L(\d+)(?:-L(\d+))?/);
            if (lineMatch) {
              const startLine = parseInt(lineMatch[1]);
              const endLine = lineMatch[2] ? parseInt(lineMatch[2]) : startLine;

              // Find requirements that fall within this line range
              for (const req of requirements) {
                const reqLineMatch = req.spec.match(/#L(\d+)/);
                if (reqLineMatch) {
                  const reqLine = parseInt(reqLineMatch[1]);
                  if (reqLine >= startLine && reqLine <= endLine && specRef.includes(req.domain)) {
                    req.tested = true;
                    if (!req.testFiles.includes(testFile)) {
                      req.testFiles.push(testFile);
                      req.lineNumbers.push(i + 1);
                    }
                  }
                }
              }
            }
          }
        }

        if (line.includes('@requirement:')) {
          const reqMatch = line.match(/\/\/\s*@requirement:\s*(.+)/);
          if (reqMatch) {
            const reqText = reqMatch[1].trim();
            // Find requirements with matching text (must have substantial overlap)
            for (const req of requirements) {
              // Extract key words from both requirement and annotation
              const reqWords = req.requirement.toLowerCase().split(/\W+/).filter(w => w.length > 3);
              const annotWords = reqText.toLowerCase().split(/\W+/).filter(w => w.length > 3);

              // Check for word overlap
              const overlap = reqWords.filter(w => annotWords.includes(w)).length;
              if (overlap >= 2) { // At least 2 meaningful words match
                req.tested = true;
                if (!req.testFiles.includes(testFile)) {
                  req.testFiles.push(testFile);
                  req.lineNumbers.push(i + 1);
                }
              }
            }
          }
        }

        // Check for message type usage in tests
        for (const msgType of messageTypes) {
          if (line.includes(`'${msgType.name}'`) || line.includes(`"${msgType.name}"`)) {
            msgType.tested = true;
            if (!msgType.testFiles.includes(testFile)) {
              msgType.testFiles.push(testFile);
              msgType.lineNumbers.push(i + 1);
            }
          }
        }

        // Check for error condition testing
        for (const error of errorConditions) {
          if (error.code && (line.includes(error.code) || line.includes(error.name))) {
            error.tested = true;
            if (!error.testFiles.includes(testFile)) {
              error.testFiles.push(testFile);
            }
          }
        }

        // Check for state transition testing
        for (const transition of stateTransitions) {
          if (line.includes(transition.event) ||
              (line.includes(transition.from) && line.includes(transition.to))) {
            transition.tested = true;
            if (!transition.testFiles.includes(testFile)) {
              transition.testFiles.push(testFile);
            }
          }
        }
      }
    } catch (error) {
      console.error(`Error reading test file ${testFile}:`, error);
    }
  }
}

/**
 * Generate markdown coverage report
 */
function generateReport(report: CoverageReport): string {
  const { requirements, messageTypes, errorConditions, stateTransitions } = report;

  // Calculate overall stats
  const totalReqs = requirements.length;
  const testedReqs = requirements.filter(r => r.tested).length;
  const coverage = totalReqs > 0 ? Math.round((testedReqs / totalReqs) * 100) : 0;

  let md = `# Signal Hub Spec Coverage Report\n\n`;
  md += `**Generated:** ${report.generatedDate}\n`;
  md += `**Total Requirements:** ${totalReqs}\n`;
  md += `**Tests Covering Requirements:** ${testedReqs}\n`;
  md += `**Coverage:** ${coverage}%\n\n`;

  // Coverage by domain
  md += `## Coverage by Domain\n\n`;
  const domains = ['connection', 'registration', 'messaging', 'pubsub'];

  for (const domain of domains) {
    const domainReqs = requirements.filter(r => r.domain === domain);
    const domainTested = domainReqs.filter(r => r.tested).length;
    const domainCoverage = domainReqs.length > 0 ? Math.round((domainTested / domainReqs.length) * 100) : 0;

    md += `### ${domain.charAt(0).toUpperCase() + domain.slice(1)} (${domain.toUpperCase()}.spec.md)\n`;
    md += `- **Requirements:** ${domainReqs.length}\n`;
    md += `- **Tested:** ${domainTested}\n`;
    md += `- **Coverage:** ${domainCoverage}%\n\n`;

    // Tested requirements
    const tested = domainReqs.filter(r => r.tested);
    if (tested.length > 0) {
      md += `#### Tested Requirements\n`;
      md += `| Requirement | Spec Reference | Tests |\n`;
      md += `|-------------|----------------|-------|\n`;
      for (const req of tested) {
        const testRefs = req.testFiles.map((f, i) => {
          const shortPath = f.replace(/.*\/(tests|src)\//, '$1/');
          return `${shortPath}:${req.lineNumbers[i]}`;
        }).join(', ');
        md += `| ${req.requirement.substring(0, 60)}${req.requirement.length > 60 ? '...' : ''} | ${req.spec} | ${testRefs} |\n`;
      }
      md += `\n`;
    }

    // Untested requirements
    const untested = domainReqs.filter(r => !r.tested);
    if (untested.length > 0) {
      md += `#### Untested Requirements\n`;
      for (const req of untested) {
        md += `- [ ] ${req.requirement} (${req.spec})\n`;
      }
      md += `\n`;
    }
  }

  // Message Types Coverage
  md += `## Message Types Coverage\n\n`;
  md += `| Message Type | Tested | Test Files | Lines |\n`;
  md += `|--------------|--------|------------|-------|\n`;
  for (const msgType of messageTypes) {
    const status = msgType.tested ? '✅' : '❌';
    const files = msgType.tested ? msgType.testFiles.map(f => f.replace(/.*\/(tests|src)\//, '$1/')).join(', ').substring(0, 40) : '-';
    const lines = msgType.tested ? msgType.lineNumbers.slice(0, 3).join(', ') + (msgType.lineNumbers.length > 3 ? '...' : '') : '-';
    md += `| ${msgType.name} | ${status} | ${files} | ${lines} |\n`;
  }
  md += `\n`;

  // Error Conditions Coverage
  md += `## Error Conditions Coverage\n\n`;
  md += `| Error Type | Code | Tested | Test Files |\n`;
  md += `|------------|------|--------|------------|\n`;
  for (const error of errorConditions) {
    const status = error.tested ? '✅' : '❌';
    const code = error.code || '-';
    const files = error.tested ? error.testFiles.map(f => f.replace(/.*\/(tests|src)\//, '$1/')).join(', ').substring(0, 40) : '-';
    md += `| ${error.name} | ${code} | ${status} | ${files} |\n`;
  }
  md += `\n`;

  // State Transition Coverage
  md += `## State Transition Coverage\n\n`;
  md += `| From | Event | To | Tested | Test Files |\n`;
  md += `|------|-------|----|---------|-----------|\n`;
  for (const transition of stateTransitions) {
    const status = transition.tested ? '✅' : '❌';
    const files = transition.tested ? transition.testFiles.map(f => f.replace(/.*\/(tests|src)\//, '$1/')).join(', ').substring(0, 40) : '-';
    md += `| ${transition.from} | ${transition.event} | ${transition.to} | ${status} | ${files} |\n`;
  }
  md += `\n`;

  // Next Steps
  md += `## Next Steps\n\n`;
  const untestedCount = requirements.filter(r => !r.tested).length;
  md += `1. Add tests for ${untestedCount} untested requirements\n`;

  const untestedMessages = messageTypes.filter(m => !m.tested);
  if (untestedMessages.length > 0) {
    md += `2. Add tests for message types: ${untestedMessages.map(m => m.name).join(', ')}\n`;
  }

  const untestedErrors = errorConditions.filter(e => !e.tested);
  if (untestedErrors.length > 0) {
    md += `3. Add tests for error conditions: ${untestedErrors.map(e => e.name).join(', ')}\n`;
  }

  const untestedTransitions = stateTransitions.filter(t => !t.tested);
  if (untestedTransitions.length > 0) {
    md += `4. Add tests for state transitions: ${untestedTransitions.map(t => `${t.from} → ${t.to}`).join(', ')}\n`;
  }

  md += `\n---\n\n`;
  md += `**Legend:**\n`;
  md += `- ✅ Tested with coverage\n`;
  md += `- ⚠️ Implemented but not tested\n`;
  md += `- ❌ Not tested\n`;

  return md;
}

/**
 * Main execution
 */
async function main() {
  console.log('🔍 Extracting requirements from spec files...');
  const requirements = extractRequirements();
  console.log(`   Found ${requirements.length} requirements`);

  console.log('🔍 Extracting message types...');
  const messageTypes = extractMessageTypes();
  console.log(`   Found ${messageTypes.length} message types`);

  console.log('🔍 Extracting error conditions...');
  const errorConditions = extractErrorConditions();
  console.log(`   Found ${errorConditions.length} error conditions`);

  console.log('🔍 Extracting state transitions...');
  const stateTransitions = extractStateTransitions();
  console.log(`   Found ${stateTransitions.length} state transitions`);

  console.log('🧪 Finding test coverage...');
  await findTestCoverage(requirements, messageTypes, errorConditions, stateTransitions);

  const testedReqs = requirements.filter(r => r.tested).length;
  const testedMsgs = messageTypes.filter(m => m.tested).length;
  console.log(`   ✅ ${testedReqs}/${requirements.length} requirements tested`);
  console.log(`   ✅ ${testedMsgs}/${messageTypes.length} message types tested`);

  console.log('📊 Generating coverage report...');
  const report: CoverageReport = {
    generatedDate: new Date().toISOString().split('T')[0],
    requirements,
    messageTypes,
    errorConditions,
    stateTransitions
  };

  const markdown = generateReport(report);
  writeFileSync(OUTPUT_FILE, markdown);
  console.log(`   ✅ Report written to ${OUTPUT_FILE}`);

  // Save requirements JSON for reference
  writeFileSync(REQUIREMENTS_FILE, JSON.stringify(report, null, 2));
  console.log(`   ✅ Requirements data written to ${REQUIREMENTS_FILE}`);

  // Summary
  const coverage = Math.round((testedReqs / requirements.length) * 100);
  console.log(`\n📈 Overall Coverage: ${coverage}%`);

  if (coverage < 80) {
    console.warn(`⚠️  Warning: Coverage below 80% threshold`);
  }
}

main().catch(console.error);
