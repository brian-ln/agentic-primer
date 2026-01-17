/**
 * Type definitions for markdown graph system
 */

import type { Node as UnistNode, Position } from 'unist';

export type MarkdownNodeType =
  | 'root'
  | 'heading'
  | 'paragraph'
  | 'text'
  | 'list'
  | 'listItem'
  | 'code'
  | 'inlineCode'
  | 'link'
  | 'image'
  | 'strong'
  | 'emphasis'
  | 'blockquote'
  | 'table'
  | 'tableRow'
  | 'tableCell'
  | 'thematicBreak'
  | 'html';

export type EdgeType =
  | 'parent-child'      // Structural containment
  | 'sibling'           // Sequential order
  | 'links-to'          // Link references
  | 'section-of'        // Heading to document
  | 'contains-code'     // Section has code
  | 'defines'           // Section defines concept
  | 'implements'        // Code implements spec
  | 'references'        // General reference
  | 'depends-on';       // Semantic dependency

export interface MarkdownNode {
  id: string;
  type: MarkdownNodeType;
  properties: {
    depth?: number;        // For headings
    lang?: string;         // For code blocks
    url?: string;          // For links/images
    alt?: string;          // For images
    title?: string;        // For links/images
    value?: string;        // For text/code/html
    ordered?: boolean;     // For lists
    start?: number;        // For ordered lists
    checked?: boolean | null; // For task list items
    [key: string]: any;
  };
  position?: Position;
  metadata: {
    document?: string;     // Source document path
    section?: string;      // Parent section name
    depth: number;         // Tree depth
    text?: string;         // Text content (cached)
  };
  children: string[];      // Child node IDs
  parent?: string;         // Parent node ID
}

export interface MarkdownEdge {
  id: string;
  fromId: string;
  toId: string;
  type: EdgeType;
  properties: {
    label?: string;
    weight?: number;
    bidirectional?: boolean;
    [key: string]: any;
  };
}

export interface NodeFilter {
  type?: MarkdownNodeType;
  depth?: number;
  lang?: string;
  textContains?: string;
  matches?: (node: MarkdownNode) => boolean;
}

export interface HeadingNode {
  id: string;
  text: string;
  depth: number;
  children: HeadingNode[];
}

export interface TOCEntry {
  text: string;
  depth: number;
  anchor: string;
  position?: Position;
}

export interface DocumentMetrics {
  nodeCount: number;
  edgeCount: number;
  headingCount: number;
  codeBlockCount: number;
  linkCount: number;
  wordCount: number;
  maxDepth: number;
}

export interface GraphDump {
  nodes: MarkdownNode[];
  edges: MarkdownEdge[];
  metadata: {
    document?: string;
    timestamp: string;
    version: string;
  };
}
