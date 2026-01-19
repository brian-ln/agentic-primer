// seag/lib/lisp-reader.ts

export type SExpr = string | SExpr[];

export function parse(input: string): SExpr[] {
  const tokens = lex(input);
  return parseExprs(tokens);
}

export function stringify(expr: SExpr): string {
  if (Array.isArray(expr)) {
    return `(${expr.map(stringify).join(" ")})`;
  }
  // Check if string needs quotes (contains spaces, parens, or starts with ")
  if (expr.includes(" ") || expr.includes("(") || expr.includes(")") || expr.startsWith('"')) {
    return JSON.stringify(expr);
  }
  return expr;
}

// --- Lexer ---

type TokenType = "LPAREN" | "RPAREN" | "STRING" | "ATOM" | "QUOTE";
interface Token { type: TokenType; value: string; pos: number; }

function lex(input: string): Token[] {
  const tokens: Token[] = [];
  let i = 0;

  while (i < input.length) {
    const char = input[i];

    // 1. Whitespace
    if (/\s/.test(char)) {
      i++;
      continue;
    }

    // 2. Comments
    if (char === ';') {
      while (i < input.length && input[i] !== '\n') i++;
      continue;
    }

    // 3. Delimiters
    if (char === '(') {
      tokens.push({ type: "LPAREN", value: "(", pos: i++ });
      continue;
    }
    if (char === ')') {
      tokens.push({ type: "RPAREN", value: ")", pos: i++ });
      continue;
    }
    if (char === "'") {
      tokens.push({ type: "QUOTE", value: "'", pos: i++ });
      continue;
    }

    // 4. Strings
    if (char === '"') {
      const start = i;
      i++; // skip open quote
      let val = "";
      while (i < input.length) {
        if (input[i] === '"') {
          break; // End of string
        }
        if (input[i] === '\\') {
          i++; // Skip escape char
          if (i >= input.length) throw new Error("Unexpected EOF in string escape");
        }
        val += input[i];
        i++;
      }
      if (i >= input.length) throw new Error("Unclosed string literal starting at " + start);
      i++; // skip close quote
      tokens.push({ type: "STRING", value: val, pos: start });
      continue;
    }

    // 5. Atoms (Symbols, Numbers, Keywords)
    // Read until whitespace or delimiter
    const start = i;
    let atom = "";
    while (i < input.length) {
      const c = input[i];
      if (/\s/.test(c) || c === '(' || c === ')' || c === ';' || c === '"') break;
      atom += c;
      i++;
    }
    tokens.push({ type: "ATOM", value: atom, pos: start });
  }

  return tokens;
}

// --- Parser ---

function parseExprs(tokens: Token[]): SExpr[] {
  let current = 0;

  function next(): SExpr {
    if (current >= tokens.length) throw new Error("Unexpected EOF");
    const token = tokens[current++];

    if (token.type === "LPAREN") {
      const list: SExpr[] = [];
      while (current < tokens.length && tokens[current].type !== "RPAREN") {
        list.push(next());
      }
      if (current >= tokens.length) throw new Error("Unclosed list");
      current++; // Consume RPAREN
      return list;
    }

    if (token.type === "RPAREN") {
      throw new Error(`Unexpected ')' at position ${token.pos}`);
    }

    if (token.type === "QUOTE") {
      return ["quote", next()];
    }

    if (token.type === "STRING" || token.type === "ATOM") {
      return token.value;
    }

    throw new Error(`Unknown token type: ${token.type}`);
  }

  const expressions: SExpr[] = [];
  while (current < tokens.length) {
    expressions.push(next());
  }
  return expressions;
}
