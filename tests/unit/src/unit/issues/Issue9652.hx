package unit.issues;
using Lambda;

class Issue9652 extends Test {
  function test() {
    var foo: Foo<Array<Int>> = [0,1];
    eq(2, foo.count());
  }
}

private abstract Foo<T>(T) from T to T {}
