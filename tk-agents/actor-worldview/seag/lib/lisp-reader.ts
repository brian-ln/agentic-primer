// seag/lib/lisp-reader.ts

export type SExpr = string | SExpr[];

export class LispReader {
  private input: string;
  private pos: number = 0;

  constructor(input: string) {
    this.input = input;
  }

  read(): SExpr[] {
    const expressions: SExpr[] = [];
    while (this.pos < this.input.length) {
      this.skipWhitespace();
      if (this.pos >= this.input.length) break;
      const expr = this.parseExpression();
      if (expr !== undefined) expressions.push(expr);
    }
    return expressions;
  }

  private parseExpression(): SExpr | undefined {
    this.skipWhitespace();
    if (this.pos >= this.input.length) return undefined;

    const char = this.input[this.pos];

    if (char === ';') {
      this.skipComment();
      return this.parseExpression();
    }

    if (char === '(') {
      this.pos++;
      const list: SExpr[] = [];
      while (true) {
        this.skipWhitespace();
        if (this.pos >= this.input.length) throw new Error("Unexpected EOF in list");
        if (this.input[this.pos] === ')') {
          this.pos++;
          return list;
        }
        const expr = this.parseExpression();
        if (expr !== undefined) list.push(expr);
      }
    }

    if (char === '"') {
      return this.parseString();
    }

    if (char === '\'') {
      this.pos++;
      const expr = this.parseExpression();
      return ["quote", expr!];
    }

    return this.parseAtom();
  }

  private parseString(): string {
    this.pos++; // skip "
    let str = "";
    while (this.pos < this.input.length) {
      const char = this.input[this.pos];
      if (char === '"') {
        this.pos++;
        return str;
      }
      if (char === '\\') {
        this.pos++;
        str += this.input[this.pos];
      } else {
        str += char;
      }
      this.pos++;
    }
    throw new Error("Unexpected EOF in string");
  }

  private parseAtom(): string {
    let atom = "";
    while (this.pos < this.input.length) {
      const char = this.input[this.pos];
      if (/\s/.test(char) || char === '(' || char === ')' || char === ';') break;
      atom += char;
      this.pos++;
    }
    return atom;
  }

  private skipWhitespace() {
    while (this.pos < this.input.length && /\s/.test(this.input[this.pos])) {
      this.pos++;
    }
  }

  private skipComment() {
    while (this.pos < this.input.length && this.input[this.pos] !== '\n') {
      this.pos++;
    }
  }
}

export function parse(input: string): SExpr[] {
  return new LispReader(input).read();
}
