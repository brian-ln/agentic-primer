#!/usr/bin/env node

/**
 * Issue Parser and Validator
 *
 * Extracts structured requirements from GitHub issues for @copilot processing.
 * Validates issue format and extracts keywords for knowledge base search.
 */

const { Octokit } = require('@octokit/rest');
const core = require('@actions/core');

async function processIssue(issueNumber, repo) {
  const octokit = new Octokit({
    auth: process.env.GITHUB_TOKEN
  });

  const [owner, repoName] = repo.split('/');

  try {
    // Fetch issue details
    const { data: issue } = await octokit.rest.issues.get({
      owner,
      repo: repoName,
      issue_number: issueNumber
    });

    console.log(`📋 Processing issue #${issueNumber}: ${issue.title}`);

    // Validate issue has required fields
    if (!issue.body || issue.body.trim().length === 0) {
      throw new Error('Issue body is empty. Please provide implementation details.');
    }

    // Extract requirements
    const requirements = extractRequirements(issue);

    // Extract acceptance criteria
    const acceptanceCriteria = extractAcceptanceCriteria(issue.body);

    // Generate keywords for KB search
    const keywords = generateKeywords(issue.title, issue.body, issue.labels);

    // Extract priority and complexity estimates
    const priority = extractPriority(issue.labels);
    const complexity = estimateComplexity(issue.body);

    // Output structured data
    const output = {
      number: issue.number,
      title: issue.title,
      author: issue.user.login,
      requirements,
      acceptanceCriteria,
      keywords,
      priority,
      complexity,
      labels: issue.labels.map(l => l.name),
      created_at: issue.created_at
    };

    // Set GitHub Actions outputs
    core.setOutput('requirements', JSON.stringify(output));
    core.setOutput('keywords', keywords.join(' '));
    core.setOutput('priority', priority);
    core.setOutput('complexity', complexity);

    console.log('✅ Issue parsed successfully');
    console.log(`   Requirements: ${requirements.length} items`);
    console.log(`   Acceptance criteria: ${acceptanceCriteria.length} items`);
    console.log(`   Keywords: ${keywords.join(', ')}`);
    console.log(`   Priority: ${priority}`);
    console.log(`   Complexity: ${complexity}`);

    return output;

  } catch (error) {
    console.error('❌ Error processing issue:', error.message);
    core.setFailed(error.message);
    process.exit(1);
  }
}

/**
 * Extract requirements from issue body
 */
function extractRequirements(issue) {
  const requirements = [];
  const body = issue.body || '';

  // Look for "Requirements:" section
  const reqSection = body.match(/#{1,3}\s*Requirements?:?\s*\n([\s\S]*?)(?=\n#{1,3}|\n\n-|$)/i);
  if (reqSection) {
    const items = reqSection[1]
      .split('\n')
      .filter(line => line.trim().match(/^[-*•]\s+/))
      .map(line => line.replace(/^[-*•]\s+/, '').trim());
    requirements.push(...items);
  }

  // Look for bullet points anywhere in body
  const bullets = body.match(/^[-*•]\s+.+$/gm);
  if (bullets) {
    bullets.forEach(bullet => {
      const req = bullet.replace(/^[-*•]\s+/, '').trim();
      if (req && !requirements.includes(req)) {
        requirements.push(req);
      }
    });
  }

  // If no structured requirements found, use title as single requirement
  if (requirements.length === 0) {
    requirements.push(issue.title);
  }

  return requirements;
}

/**
 * Extract acceptance criteria (checkboxes)
 */
function extractAcceptanceCriteria(body) {
  const criteria = [];

  // Match checkbox items: - [ ] or - [x]
  const checkboxes = body.match(/^-\s+\[([ x])\]\s+(.+)$/gm);

  if (checkboxes) {
    checkboxes.forEach(checkbox => {
      const match = checkbox.match(/^-\s+\[([ x])\]\s+(.+)$/);
      if (match) {
        criteria.push({
          checked: match[1] === 'x',
          text: match[2].trim()
        });
      }
    });
  }

  return criteria;
}

/**
 * Generate keywords for knowledge base search
 */
function generateKeywords(title, body, labels) {
  const keywords = new Set();

  // Add words from title (weighted higher)
  const titleWords = extractWords(title);
  titleWords.forEach(word => keywords.add(word));

  // Add label names
  labels.forEach(label => {
    const labelWords = extractWords(label.name);
    labelWords.forEach(word => keywords.add(word));
  });

  // Add important words from body
  const bodyWords = extractWords(body);
  const importantWords = bodyWords.filter(word => {
    // Keep longer words and technical terms
    return word.length > 4 || isTechnicalTerm(word);
  });
  importantWords.slice(0, 10).forEach(word => keywords.add(word));

  return Array.from(keywords);
}

/**
 * Extract meaningful words from text
 */
function extractWords(text) {
  if (!text) return [];

  const stopWords = new Set([
    'the', 'be', 'to', 'of', 'and', 'a', 'in', 'that', 'have',
    'i', 'it', 'for', 'not', 'on', 'with', 'he', 'as', 'you',
    'do', 'at', 'this', 'but', 'his', 'by', 'from', 'they',
    'we', 'say', 'her', 'she', 'or', 'an', 'will', 'my', 'one',
    'all', 'would', 'there', 'their', 'what', 'so', 'up', 'out',
    'if', 'about', 'who', 'get', 'which', 'go', 'me', 'when',
    'make', 'can', 'like', 'time', 'no', 'just', 'him', 'know'
  ]);

  return text
    .toLowerCase()
    .replace(/[^\w\s-]/g, ' ')
    .split(/\s+/)
    .filter(word => word.length > 2 && !stopWords.has(word))
    .filter(word => !/^\d+$/.test(word)); // Remove pure numbers
}

/**
 * Check if word is a technical term
 */
function isTechnicalTerm(word) {
  const technicalPatterns = [
    /^api$/i,
    /^auth/i,
    /^jwt$/i,
    /^oauth/i,
    /^sql$/i,
    /^http/i,
    /^rest$/i,
    /^json$/i,
    /^xml$/i,
    /^db$/i,
    /^ui$/i,
    /^ux$/i,
    /endpoint/i,
    /^css$/i,
    /^html$/i,
    /^js$/i,
    /^ts$/i
  ];

  return technicalPatterns.some(pattern => pattern.test(word));
}

/**
 * Extract priority from labels
 */
function extractPriority(labels) {
  const priorities = {
    'priority: critical': 0,
    'priority: high': 1,
    'priority: medium': 2,
    'priority: low': 3,
    'p0': 0,
    'p1': 1,
    'p2': 2,
    'p3': 3
  };

  for (const label of labels) {
    const labelName = label.name.toLowerCase();
    if (priorities.hasOwnProperty(labelName)) {
      return priorities[labelName];
    }
  }

  return 2; // Default to medium
}

/**
 * Estimate implementation complexity
 */
function estimateComplexity(body) {
  // Simple heuristic based on content length and keywords
  const length = body.length;
  const requirementsCount = (body.match(/^[-*•]\s+/gm) || []).length;
  const complexityKeywords = [
    'integrate', 'migration', 'refactor', 'architecture',
    'security', 'authentication', 'performance', 'scalability',
    'database', 'api', 'microservice', 'distributed'
  ];

  let score = 0;

  // Length factor
  if (length > 1000) score += 3;
  else if (length > 500) score += 2;
  else score += 1;

  // Requirements count
  if (requirementsCount > 5) score += 2;
  else if (requirementsCount > 2) score += 1;

  // Technical complexity keywords
  const lowerBody = body.toLowerCase();
  const keywordMatches = complexityKeywords.filter(kw =>
    lowerBody.includes(kw)
  ).length;
  score += Math.min(keywordMatches, 3);

  // Map score to complexity level
  if (score >= 7) return 'high';
  if (score >= 4) return 'medium';
  return 'low';
}

// Parse command line arguments
const args = process.argv.slice(2);
let issueNumber, repo;

for (let i = 0; i < args.length; i += 2) {
  const flag = args[i];
  const value = args[i + 1];

  if (flag === '--issue-number') issueNumber = parseInt(value);
  if (flag === '--repo') repo = value;
}

if (!issueNumber || !repo) {
  console.error('Usage: process-issue.js --issue-number <num> --repo <owner/repo>');
  process.exit(1);
}

// Run
processIssue(issueNumber, repo);
