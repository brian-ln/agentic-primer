#!/usr/bin/env node

/**
 * Knowledge Base Search Engine
 *
 * Performs TF-IDF based semantic search across markdown documentation
 * to find relevant context for issue implementation.
 */

const fs = require('fs');
const path = require('path');
const core = require('@actions/core');

class KnowledgeBaseSearch {
  constructor(kbPath) {
    this.kbPath = kbPath;
    this.documents = [];
    this.idfScores = {};
  }

  /**
   * Load all markdown documents from knowledge base
   */
  loadDocuments() {
    console.log(`📚 Loading knowledge base from ${this.kbPath}`);

    const files = this.findMarkdownFiles(this.kbPath);

    files.forEach(filePath => {
      const content = fs.readFileSync(filePath, 'utf8');
      const category = this.extractCategory(filePath);
      const title = this.extractTitle(content);

      this.documents.push({
        path: filePath,
        relativePath: path.relative(this.kbPath, filePath),
        category,
        title,
        content,
        sections: this.extractSections(content),
        words: this.extractWords(content)
      });
    });

    console.log(`   Loaded ${this.documents.length} documents`);

    // Compute IDF scores
    this.computeIDF();
  }

  /**
   * Recursively find all markdown files
   */
  findMarkdownFiles(dir) {
    const files = [];

    if (!fs.existsSync(dir)) {
      console.warn(`⚠️  Knowledge base directory not found: ${dir}`);
      return files;
    }

    const entries = fs.readdirSync(dir, { withFileTypes: true });

    entries.forEach(entry => {
      const fullPath = path.join(dir, entry.name);

      if (entry.isDirectory()) {
        files.push(...this.findMarkdownFiles(fullPath));
      } else if (entry.isFile() && entry.name.endsWith('.md')) {
        files.push(fullPath);
      }
    });

    return files;
  }

  /**
   * Extract category from file path
   */
  extractCategory(filePath) {
    const parts = filePath.split(path.sep);
    const kbIndex = parts.findIndex(p => p === 'knowledge');

    if (kbIndex >= 0 && kbIndex < parts.length - 1) {
      return parts[kbIndex + 1];
    }

    return 'general';
  }

  /**
   * Extract title from markdown content
   */
  extractTitle(content) {
    const match = content.match(/^#\s+(.+)$/m);
    return match ? match[1].trim() : 'Untitled';
  }

  /**
   * Extract sections from markdown
   */
  extractSections(content) {
    const sections = [];
    const lines = content.split('\n');
    let currentSection = null;

    lines.forEach(line => {
      const headerMatch = line.match(/^(#{2,6})\s+(.+)$/);

      if (headerMatch) {
        if (currentSection) {
          sections.push(currentSection);
        }

        currentSection = {
          level: headerMatch[1].length,
          title: headerMatch[2].trim(),
          content: ''
        };
      } else if (currentSection) {
        currentSection.content += line + '\n';
      }
    });

    if (currentSection) {
      sections.push(currentSection);
    }

    return sections;
  }

  /**
   * Extract and normalize words from text
   */
  extractWords(text) {
    const stopWords = new Set([
      'the', 'be', 'to', 'of', 'and', 'a', 'in', 'that', 'have',
      'i', 'it', 'for', 'not', 'on', 'with', 'as', 'you',
      'do', 'at', 'this', 'but', 'by', 'from', 'they'
    ]);

    return text
      .toLowerCase()
      .replace(/[^\w\s-]/g, ' ')
      .split(/\s+/)
      .filter(word => word.length > 2 && !stopWords.has(word));
  }

  /**
   * Compute IDF (Inverse Document Frequency) for all terms
   */
  computeIDF() {
    const documentCount = this.documents.length;
    const termDocumentCount = {};

    // Count how many documents contain each term
    this.documents.forEach(doc => {
      const uniqueWords = new Set(doc.words);
      uniqueWords.forEach(word => {
        termDocumentCount[word] = (termDocumentCount[word] || 0) + 1;
      });
    });

    // Calculate IDF: log(N / df)
    Object.keys(termDocumentCount).forEach(term => {
      this.idfScores[term] = Math.log(
        documentCount / termDocumentCount[term]
      );
    });
  }

  /**
   * Search for relevant documents
   */
  search(query, maxResults = 5) {
    console.log(`🔍 Searching knowledge base for: "${query}"`);

    if (this.documents.length === 0) {
      this.loadDocuments();
    }

    const queryWords = this.extractWords(query);
    const results = [];

    this.documents.forEach(doc => {
      const score = this.computeTFIDF(queryWords, doc);

      if (score > 0) {
        results.push({
          document: doc,
          score,
          matchedTerms: this.getMatchedTerms(queryWords, doc.words)
        });
      }
    });

    // Sort by score descending
    results.sort((a, b) => b.score - a.score);

    const topResults = results.slice(0, maxResults);

    console.log(`   Found ${results.length} matches, returning top ${topResults.length}`);

    return topResults;
  }

  /**
   * Compute TF-IDF score for document against query
   */
  computeTFIDF(queryWords, document) {
    let score = 0;
    const docWordCount = document.words.length;

    queryWords.forEach(queryWord => {
      // Term Frequency: count of term in document / total words
      const termCount = document.words.filter(w => w === queryWord).length;
      const tf = termCount / docWordCount;

      // IDF score
      const idf = this.idfScores[queryWord] || 0;

      // TF-IDF
      score += tf * idf;
    });

    // Boost score for category matches
    if (queryWords.some(w => document.category.includes(w))) {
      score *= 1.5;
    }

    // Boost score for title matches
    const titleWords = this.extractWords(document.title);
    const titleMatches = queryWords.filter(w => titleWords.includes(w)).length;
    score += titleMatches * 0.5;

    return score;
  }

  /**
   * Get terms that matched between query and document
   */
  getMatchedTerms(queryWords, docWords) {
    const docWordSet = new Set(docWords);
    return queryWords.filter(w => docWordSet.has(w));
  }

  /**
   * Format results for output
   */
  formatResults(results) {
    const formatted = results.map(result => {
      const doc = result.document;

      // Find most relevant section
      const bestSection = this.findBestSection(result.matchedTerms, doc.sections);

      return {
        path: doc.relativePath,
        category: doc.category,
        title: doc.title,
        score: result.score.toFixed(3),
        matchedTerms: result.matchedTerms,
        section: bestSection ? bestSection.title : null,
        excerpt: bestSection ? this.createExcerpt(bestSection.content, 200) : null
      };
    });

    return formatted;
  }

  /**
   * Find section with most query term matches
   */
  findBestSection(matchedTerms, sections) {
    if (sections.length === 0) return null;

    let bestSection = sections[0];
    let bestScore = 0;

    sections.forEach(section => {
      const sectionWords = this.extractWords(section.content);
      const matches = matchedTerms.filter(term =>
        sectionWords.includes(term)
      ).length;

      if (matches > bestScore) {
        bestScore = matches;
        bestSection = section;
      }
    });

    return bestSection;
  }

  /**
   * Create excerpt from text
   */
  createExcerpt(text, maxLength) {
    const cleaned = text.trim().replace(/\s+/g, ' ');

    if (cleaned.length <= maxLength) {
      return cleaned;
    }

    return cleaned.substring(0, maxLength) + '...';
  }
}

/**
 * Main execution
 */
async function main() {
  const args = process.argv.slice(2);
  let query, kbPath;

  for (let i = 0; i < args.length; i += 2) {
    const flag = args[i];
    const value = args[i + 1];

    if (flag === '--query') query = value;
    if (flag === '--kb-path') kbPath = value;
  }

  if (!query || !kbPath) {
    console.error('Usage: kb-search.js --query <search query> --kb-path <path>');
    process.exit(1);
  }

  const searcher = new KnowledgeBaseSearch(kbPath);
  const results = searcher.search(query);
  const formatted = searcher.formatResults(results);

  // Output results
  console.log('\n📋 Search Results:');
  formatted.forEach((result, index) => {
    console.log(`\n${index + 1}. ${result.title} (${result.category})`);
    console.log(`   Path: ${result.path}`);
    console.log(`   Score: ${result.score}`);
    console.log(`   Matched: ${result.matchedTerms.join(', ')}`);
    if (result.section) {
      console.log(`   Section: ${result.section}`);
    }
    if (result.excerpt) {
      console.log(`   Excerpt: ${result.excerpt}`);
    }
  });

  // Set GitHub Actions outputs
  core.setOutput('context', JSON.stringify(formatted));
  core.setOutput('summary', formatted.map(r =>
    `${r.category}/${r.title}: ${r.section || 'General guidance'}`
  ).join('; '));

  console.log('\n✅ Knowledge base search complete');
}

// Run
main().catch(error => {
  console.error('❌ Error:', error.message);
  core.setFailed(error.message);
  process.exit(1);
});
