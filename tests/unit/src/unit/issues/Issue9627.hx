package unit.issues;

class Issue9627 extends Test {
  function test() {
    var bar: Bar = {foo: [0]};
    var matches = switch (bar) {
      case {foo: {it: [0]}}: true;
      case _: false;
    }
    t(matches);
  }
}

private abstract Foo<T>(T) from T to T {
  public var it(get, never): T;
  private inline function get_it() return this;
}

@:forward
private abstract Bar({foo: Array<Int>}) from {foo: Array<Int>} to {foo: Array<Int>} {
  public var foo(get, never): Foo<Array<Int>>;
  private inline function get_foo() return this.foo;
}
