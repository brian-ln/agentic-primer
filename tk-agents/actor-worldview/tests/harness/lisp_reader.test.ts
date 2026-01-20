import { expect, test, describe } from "bun:test";
import { parse, SExpr } from "../../seag/lib/lisp-reader";
import { System, Actor, Message, ActorAddress } from "../../seag/kernel";

describe("Lisp Reader", () => {
  // Helper actor to send messages in tests
  class TestClientActor extends Actor {
    public receivedMessages: Message[] = [];
    constructor(id: string, system: System) {
      super(id, system);
    }
    async receive(msg: Message) {
      this.receivedMessages.push(msg);
    }
    // Helper to send a message and wait for a response if needed
    sendAndWait(target: ActorAddress, msg: Message, timeout = 100) {
      return new Promise<Message | null>(resolve => {
        this.receivedMessages = []; // Clear previous messages
        this.send(target, { ...msg, sender: this.id });
        const timer = setTimeout(() => resolve(null), timeout);
        const check = setInterval(() => {
          if (this.receivedMessages.length > 0) {
            clearInterval(check);
            clearTimeout(timer);
            resolve(this.receivedMessages[0]);
          }
        }, 10);
      });
    }
  }

  test("parses atoms", () => {
    expect(parse("hello")).toBe("hello");
    expect(parse("123")).toBe(123);
    expect(parse("true")).toBe(true);
    expect(parse(":keyword")).toBe(":keyword");
  });

  test("parses lists", () => {
    expect(parse("(a b c)")).toEqual(["a", "b", "c"]);
    expect(parse("(+ 1 2)")).toEqual(["+", 1, 2]);
  });

  test("ignores comments", () => {
    expect(parse("(a b ; comment\n c)")).toEqual(["a", "b", "c"]);
  });

  test("handles strings", () => {
    expect(parse("(hello \"world\")")).toEqual(["hello", "world"]);
  });

  test("handles curly braces as lists", () => {
    expect(parse("{a b c}")).toEqual(["a", "b", "c"]);
  });

  test("handles mixed nesting", () => {
    const sexpr = "(defprotocol Foo (on Bar (arg1) (seq (one Baz (res))))) \
                   (actor MyActor (implements Foo) (behavior (on Bar (arg) (send self 'Baz)))))";
    const expected = [
      "defprotocol", "Foo", ["on", "Bar", ["arg1"], ["seq", ["one", "Baz", ["res"]]]],
      "actor", "MyActor", ["implements", "Foo"], ["behavior", ["on", "Bar", ["arg"], ["send", "self", "Baz"]]]
    ];
    expect(parse(sexpr)).toEqual(expected);
  });
});