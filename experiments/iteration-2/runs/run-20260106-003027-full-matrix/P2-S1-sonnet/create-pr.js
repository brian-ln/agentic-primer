#!/usr/bin/env node

/**
 * Pull Request Creator
 *
 * Creates well-formatted pull requests with automatic issue linking,
 * detailed descriptions, and proper labeling.
 */

const { Octokit } = require('@octokit/rest');
const core = require('@actions/core');

class PullRequestCreator {
  constructor(octokit, owner, repo) {
    this.octokit = octokit;
    this.owner = owner;
    this.repo = repo;
  }

  /**
   * Create pull request from implementation
   */
  async createPR(issueNumber, branch, plan, kbContext) {
    console.log(`📝 Creating pull request for issue #${issueNumber}`);

    // Fetch issue details
    const { data: issue } = await this.octokit.rest.issues.get({
      owner: this.owner,
      repo: this.repo,
      issue_number: issueNumber
    });

    // Generate PR title
    const title = this.generateTitle(issue);

    // Generate PR body
    const body = this.generateBody(issue, plan, kbContext);

    // Create the PR
    const { data: pr } = await this.octokit.rest.pulls.create({
      owner: this.owner,
      repo: this.repo,
      title,
      head: branch,
      base: 'main', // TODO: Make configurable
      body,
      draft: true // Start as draft for review
    });

    console.log(`✅ Created PR #${pr.number}: ${title}`);

    return pr;
  }

  /**
   * Generate PR title from issue
   */
  generateTitle(issue) {
    // Use issue title, but clean it up
    let title = issue.title.trim();

    // Remove common prefixes
    title = title.replace(/^(feat|feature|fix|bug|docs|refactor|test|chore):\s*/i, '');

    // Add appropriate prefix based on labels
    const labels = issue.labels.map(l => l.name.toLowerCase());

    if (labels.includes('bug') || labels.includes('fix')) {
      return `fix: ${title}`;
    }

    if (labels.includes('documentation') || labels.includes('docs')) {
      return `docs: ${title}`;
    }

    if (labels.includes('refactor')) {
      return `refactor: ${title}`;
    }

    if (labels.includes('test') || labels.includes('testing')) {
      return `test: ${title}`;
    }

    // Default to feature
    return `feat: ${title}`;
  }

  /**
   * Generate comprehensive PR body
   */
  generateBody(issue, plan, kbContext) {
    const sections = [];

    // Summary section
    sections.push('## Summary\n');
    sections.push(plan.summary || issue.title);
    sections.push('');

    // Changes section
    sections.push('## Changes\n');

    if (plan.files && plan.files.length > 0) {
      plan.files.forEach(file => {
        const icon = this.getFileIcon(file.type);
        sections.push(`- ${icon} \`${file.path}\` - ${file.purpose}`);
      });
    } else {
      sections.push('- Implementation details in commits');
    }
    sections.push('');

    // Technical details
    if (plan.techStack) {
      sections.push('## Technical Details\n');
      sections.push('**Tech Stack:**');
      Object.entries(plan.techStack).forEach(([key, value]) => {
        if (value && value !== 'Unknown' && value !== 'None') {
          sections.push(`- ${key}: ${value}`);
        }
      });
      sections.push('');
    }

    // Dependencies
    if (plan.dependencies && plan.dependencies.length > 0) {
      sections.push('## Dependencies\n');
      const prodDeps = plan.dependencies.filter(d => !d.devDependency);
      const devDeps = plan.dependencies.filter(d => d.devDependency);

      if (prodDeps.length > 0) {
        sections.push('**Production:**');
        prodDeps.forEach(dep => {
          sections.push(`- \`${dep.name}@${dep.version}\` - ${dep.purpose}`);
        });
      }

      if (devDeps.length > 0) {
        sections.push('\n**Development:**');
        devDeps.forEach(dep => {
          sections.push(`- \`${dep.name}@${dep.version}\` - ${dep.purpose}`);
        });
      }
      sections.push('');
    }

    // Testing
    if (plan.testing) {
      sections.push('## Testing\n');

      if (plan.testing.unit && plan.testing.unit.length > 0) {
        sections.push('**Unit Tests:**');
        plan.testing.unit.forEach(test => {
          sections.push(`- ${test.target}`);
          test.scenarios.forEach(scenario => {
            sections.push(`  - ${scenario}`);
          });
        });
        sections.push('');
      }

      if (plan.testing.integration && plan.testing.integration.length > 0) {
        sections.push('**Integration Tests:**');
        plan.testing.integration.forEach(test => {
          sections.push(`- ${test.target}`);
          test.scenarios.forEach(scenario => {
            sections.push(`  - ${scenario}`);
          });
        });
        sections.push('');
      }
    }

    // Knowledge base context
    if (kbContext && kbContext.length > 0) {
      sections.push('## Knowledge Base Context\n');
      sections.push('This implementation follows our established patterns and standards:');
      kbContext.slice(0, 3).forEach(ctx => {
        sections.push(`- **${ctx.category}/${ctx.title}**: ${ctx.section || 'General guidance'}`);
      });
      sections.push('');
    }

    // Risks
    if (plan.risks && plan.risks.length > 0) {
      sections.push('## Risks & Mitigations\n');
      plan.risks.forEach(risk => {
        const icon = risk.level === 'high' ? '🔴' : risk.level === 'medium' ? '🟡' : '🟢';
        sections.push(`${icon} **${risk.description}**`);
        sections.push(`   - Mitigation: ${risk.mitigation}`);
        sections.push('');
      });
    }

    // Checklist for reviewer
    sections.push('## Review Checklist\n');
    sections.push('- [ ] Code follows project coding standards');
    sections.push('- [ ] Tests pass locally');
    sections.push('- [ ] No security vulnerabilities introduced');
    sections.push('- [ ] Documentation updated');
    sections.push('- [ ] Ready to merge');
    sections.push('');

    // Issue link (enables auto-close)
    sections.push(`## Related Issues\n`);
    sections.push(`Closes #${issue.number}`);
    sections.push('');

    // Footer
    sections.push('---');
    sections.push('');
    sections.push('🤖 *This PR was generated by @copilot*');

    if (plan.estimatedHours) {
      sections.push(`⏱️  *Estimated effort: ${plan.estimatedHours} hours*`);
    }

    return sections.join('\n');
  }

  /**
   * Get emoji icon for file type
   */
  getFileIcon(type) {
    const icons = {
      route: '🛣️',
      service: '⚙️',
      model: '📊',
      middleware: '🔀',
      util: '🛠️',
      test: '🧪',
      documentation: '📚'
    };

    return icons[type] || '📄';
  }
}

/**
 * Main execution
 */
async function main() {
  const args = process.argv.slice(2);
  let issueNumber, branch, plan, kbContext;

  for (let i = 0; i < args.length; i += 2) {
    const flag = args[i];
    const value = args[i + 1];

    if (flag === '--issue-number') issueNumber = parseInt(value);
    if (flag === '--branch') branch = value;
    if (flag === '--plan') plan = JSON.parse(value);
    if (flag === '--kb-context') kbContext = JSON.parse(value);
  }

  if (!issueNumber || !branch) {
    console.error('Usage: create-pr.js --issue-number <num> --branch <name> --plan <json> --kb-context <json>');
    process.exit(1);
  }

  const octokit = new Octokit({
    auth: process.env.GITHUB_TOKEN
  });

  const repo = process.env.GITHUB_REPOSITORY;
  const [owner, repoName] = repo.split('/');

  const creator = new PullRequestCreator(octokit, owner, repoName);

  try {
    const pr = await creator.createPR(issueNumber, branch, plan || {}, kbContext || []);

    // Set outputs
    core.setOutput('pr-number', pr.number);
    core.setOutput('pr-url', pr.html_url);

    console.log(`\n✅ Pull request created successfully`);
    console.log(`   URL: ${pr.html_url}`);

  } catch (error) {
    console.error('❌ Error creating PR:', error.message);
    core.setFailed(error.message);
    process.exit(1);
  }
}

// Run
main();
