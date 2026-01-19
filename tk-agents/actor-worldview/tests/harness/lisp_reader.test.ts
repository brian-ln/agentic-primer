import { expect, test, describe } from "bun:test";
import { parse } from "../../seag/lib/lisp-reader";

describe("Lisp Reader", () => {
  test("parses atoms", () => {
    const ast = parse("foo bar 123");
    expect(ast).toEqual(["foo", "bar", "123"]);
  });

  test("parses lists", () => {
    const ast = parse("(a b (c))");
    expect(ast).toEqual([["a", "b", ["c"]]]);
  });

  test("ignores comments", () => {
    const ast = parse(`
      (a b) ; comment
      ;; line comment
      (c)
    `);
    expect(ast).toEqual([["a", "b"], ["c"]]);
  });

  test("handles strings", () => {
    const ast = parse('(print "hello world")');
    expect(ast).toEqual([["print", "hello world"]]);
  });

  test("handles mixed nesting", () => {
    const code = `
      (system SEAG
        (actors
          (actor A (on msg (arg) (do-something arg)))))
    `;
    const ast = parse(code);
    expect(ast.length).toBe(1);
    const system = ast[0] as any[];
    expect(system[0]).toBe("system");
    expect(system[1]).toBe("SEAG");
  });
});
