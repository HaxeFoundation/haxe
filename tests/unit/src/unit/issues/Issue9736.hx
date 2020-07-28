package unit.issues;

import haxe.Constraints.Function;

class Issue9736 extends Test {
  function test() {
    ((null: Foo): Function);
    utest.Assert.pass();
  }
}

private abstract Foo(Function) from Function to Function {}
