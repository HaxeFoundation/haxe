package unit.issues;

private enum FooBar {
  Foo;
  Bar(value: Any);
}

class Issue10148 extends Test {
  function test() {
    var bar: FooBar = Bar(0);
    var matched = switch (bar) {
      case Bar(0): true;
      case _: false;
    }
    eq(true, matched);
  }
}
