#!/usr/bin/env node
/**
 * Report Generator - Generates unified test reports from all runners
 *
 * Aggregates results from BDD, FIT, and State Machine tests into
 * a comprehensive report showing overall test coverage and gaps.
 */

import { writeFileSync, mkdirSync, existsSync } from 'fs';
import { join, dirname } from 'path';
import { fileURLToPath } from 'url';
import { execSync } from 'child_process';

const __dirname = dirname(fileURLToPath(import.meta.url));

/**
 * Run all test runners and capture results
 */
async function runAllTests() {
  const results = {
    timestamp: new Date().toISOString(),
    bdd: { passed: 0, failed: 0, total: 0, details: '' },
    fit: { passed: 0, failed: 0, total: 0, details: '' },
    state: { passed: 0, failed: 0, total: 0, details: '' }
  };

  console.log('Running all specification tests...\n');

  // Run BDD tests
  console.log('1. Running BDD tests...');
  try {
    const bddOutput = execSync('node specs/runners/bdd-runner.js', {
      cwd: join(__dirname, '../..'),
      encoding: 'utf-8'
    });
    results.bdd.details = bddOutput;

    // Parse output for pass/fail counts
    const passMatch = bddOutput.match(/Scenarios passed:\s*(\d+)/);
    const failMatch = bddOutput.match(/Scenarios failed:\s*(\d+)/);
    const totalMatch = bddOutput.match(/Total scenarios:\s*(\d+)/);

    if (passMatch) results.bdd.passed = parseInt(passMatch[1], 10);
    if (failMatch) results.bdd.failed = parseInt(failMatch[1], 10);
    if (totalMatch) results.bdd.total = parseInt(totalMatch[1], 10);
  } catch (error) {
    results.bdd.details = error.stdout || error.stderr || 'BDD tests failed to run';
    results.bdd.failed = results.bdd.total; // Assume all failed if crash
  }

  // Run FIT tests
  console.log('2. Running FIT tests...');
  try {
    const fitOutput = execSync('node specs/runners/fit-runner.js', {
      cwd: join(__dirname, '../..'),
      encoding: 'utf-8'
    });
    results.fit.details = fitOutput;

    // Parse output
    const passMatch = fitOutput.match(/Rows passed:\s*(\d+)/);
    const failMatch = fitOutput.match(/Rows failed:\s*(\d+)/);

    if (passMatch) results.fit.passed = parseInt(passMatch[1], 10);
    if (failMatch) results.fit.failed = parseInt(failMatch[1], 10);
    results.fit.total = results.fit.passed + results.fit.failed;
  } catch (error) {
    results.fit.details = error.stdout || error.stderr || 'FIT tests failed to run';
    results.fit.failed = results.fit.total;
  }

  // Run State Machine validation
  console.log('3. Running State Machine validation...');
  try {
    const stateOutput = execSync('node specs/runners/state-validator.js', {
      cwd: join(__dirname, '../..'),
      encoding: 'utf-8'
    });
    results.state.details = stateOutput;

    // Parse output
    const validMatch = stateOutput.match(/Validated:\s*(\d+)/);
    const failMatch = stateOutput.match(/Failed:\s*(\d+)/);

    if (validMatch) results.state.passed = parseInt(validMatch[1], 10);
    if (failMatch) results.state.failed = parseInt(failMatch[1], 10);
    results.state.total = results.state.passed + results.state.failed;
  } catch (error) {
    results.state.details = error.stdout || error.stderr || 'State validation failed to run';
    results.state.failed = results.state.total;
  }

  return results;
}

/**
 * Generate markdown report
 */
function generateMarkdownReport(results) {
  const totalPassed = results.bdd.passed + results.fit.passed + results.state.passed;
  const totalFailed = results.bdd.failed + results.fit.failed + results.state.failed;
  const totalTests = results.bdd.total + results.fit.total + results.state.total;
  const passRate = totalTests > 0 ? ((totalPassed / totalTests) * 100).toFixed(1) : 0;

  let report = `# Event System Specification Test Report

Generated: ${results.timestamp}

## Executive Summary

- **Total Tests**: ${totalTests}
- **Passed**: ${totalPassed} (${passRate}%)
- **Failed**: ${totalFailed}
- **Pass Rate**: ${passRate >= 80 ? '✅' : '⚠️'} ${passRate}% ${passRate >= 80 ? '(Target: 80%+)' : '(Below 80% target)'}

## Test Breakdown

### BDD Scenarios (Gherkin Features)

- Total Scenarios: ${results.bdd.total}
- Passed: ${results.bdd.passed}
- Failed: ${results.bdd.failed}
- Pass Rate: ${results.bdd.total > 0 ? ((results.bdd.passed / results.bdd.total) * 100).toFixed(1) : 0}%

### FIT Decision Tables

- Total Test Rows: ${results.fit.total}
- Passed: ${results.fit.passed}
- Failed: ${results.fit.failed}
- Pass Rate: ${results.fit.total > 0 ? ((results.fit.passed / results.fit.total) * 100).toFixed(1) : 0}%

### State Machine Validation

- Total State Machines: ${results.state.total}
- Validated: ${results.state.passed}
- Failed: ${results.state.failed}
- Pass Rate: ${results.state.total > 0 ? ((results.state.passed / results.state.total) * 100).toFixed(1) : 0}%

## Coverage Analysis

### Implementation Status

`;

  // Analyze coverage based on results
  if (results.bdd.passed > 0) {
    report += `- ✅ BDD scenarios are being executed\n`;
  } else {
    report += `- ⚠️ BDD scenarios not yet implemented\n`;
  }

  if (results.fit.passed > 0) {
    report += `- ✅ FIT decision tables are being executed\n`;
  } else {
    report += `- ⚠️ FIT decision tables not yet implemented\n`;
  }

  if (results.state.passed > 0) {
    report += `- ✅ State machine validation is working\n`;
  } else {
    report += `- ⚠️ State machine validation not yet implemented\n`;
  }

  report += `\n### Gaps and Next Steps

`;

  if (totalFailed > 0) {
    report += `${totalFailed} tests are currently failing. Key areas to address:

`;

    if (results.bdd.failed > 0) {
      report += `1. **BDD Scenarios**: ${results.bdd.failed} scenarios failing\n`;
      report += `   - Review step definitions and actor implementations\n`;
      report += `   - Check for missing features or incorrect behavior\n\n`;
    }

    if (results.fit.failed > 0) {
      report += `2. **FIT Tables**: ${results.fit.failed} test rows failing\n`;
      report += `   - Verify decision table logic against implementation\n`;
      report += `   - Ensure all table rows have corresponding test code\n\n`;
    }

    if (results.state.failed > 0) {
      report += `3. **State Machines**: ${results.state.failed} validations failing\n`;
      report += `   - Check state transition logic\n`;
      report += `   - Verify invariants and guards\n\n`;
    }
  } else {
    report += `All tests passing! The specification and implementation are aligned.\n\n`;
  }

  report += `## Detailed Output

### BDD Runner Output

\`\`\`
${results.bdd.details.substring(0, 2000)}${results.bdd.details.length > 2000 ? '...\n(truncated)' : ''}
\`\`\`

### FIT Runner Output

\`\`\`
${results.fit.details.substring(0, 2000)}${results.fit.details.length > 2000 ? '...\n(truncated)' : ''}
\`\`\`

### State Validator Output

\`\`\`
${results.state.details.substring(0, 2000)}${results.state.details.length > 2000 ? '...\n(truncated)' : ''}
\`\`\`

## Recommendations

`;

  if (passRate >= 80) {
    report += `The ${passRate}% pass rate meets the 80% success target. Continue implementing remaining features and improving test coverage.\n\n`;
  } else {
    report += `The ${passRate}% pass rate is below the 80% target. Priority should be:\n\n`;
    report += `1. Focus on the test categories with lowest pass rates\n`;
    report += `2. Implement missing actor behaviors\n`;
    report += `3. Fix failing test assertions\n`;
    report += `4. Re-run specs after each fix to track progress\n\n`;
  }

  report += `## Usage

To re-run these tests:

\`\`\`bash
# Run all specs
bun run specs:all

# Run individual spec types
bun run specs:bdd
bun run specs:fit
bun run specs:states

# Generate new report
bun run specs:report
\`\`\`

## Files

- BDD Features: \`specs/features/*.feature\`
- FIT Tables: \`specs/fit-fixtures/*.fit.md\`
- State Machines: \`specs/state-machines/*-state-machine.md\`
- Test Runners: \`specs/runners/*.js\`

---

*Report generated by report-generator.js*
`;

  return report;
}

/**
 * Main execution
 */
async function main() {
  console.log('Event System Specification Report Generator\n');
  console.log('='.repeat(60));
  console.log();

  // Run all tests
  const results = await runAllTests();

  // Generate report
  const report = generateMarkdownReport(results);

  // Save report to file
  const reportsDir = join(__dirname, '../../reports');
  if (!existsSync(reportsDir)) {
    mkdirSync(reportsDir, { recursive: true });
  }

  const reportFile = join(reportsDir, 'spec-test-report.md');
  writeFileSync(reportFile, report);

  console.log('\n' + '='.repeat(60));
  console.log(`\nReport saved to: ${reportFile}\n`);

  // Print summary to console
  const totalPassed = results.bdd.passed + results.fit.passed + results.state.passed;
  const totalFailed = results.bdd.failed + results.fit.failed + results.state.failed;
  const totalTests = results.bdd.total + results.fit.total + results.state.total;
  const passRate = totalTests > 0 ? ((totalPassed / totalTests) * 100).toFixed(1) : 0;

  console.log('Summary:');
  console.log(`  Total Tests: ${totalTests}`);
  console.log(`  Passed: ${totalPassed}`);
  console.log(`  Failed: ${totalFailed}`);
  console.log(`  Pass Rate: ${passRate}%`);
  console.log();

  if (passRate >= 80) {
    console.log('✅ Pass rate meets 80% target!');
  } else {
    console.log(`⚠️  Pass rate below 80% target (current: ${passRate}%)`);
  }

  process.exit(0);
}

if (import.meta.url === `file://${process.argv[1]}`) {
  main().catch(console.error);
}

export { runAllTests, generateMarkdownReport };
