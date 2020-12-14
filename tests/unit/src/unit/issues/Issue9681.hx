package unit.issues;

using Lambda;

class Issue9681 extends Test {
  function test() {
    var array = [0,1];

    var foo: Foo<Array<Int>> = array;
    eq(false, foo.empty());
    eq(2, foo.count());

    var bar: Bar<Array<Int>> = array;
    eq(false, bar.empty());
    eq(0, bar.count());
  }

  public static function count<T>(array: Array<T>)
    return 0;
}

private abstract Foo<T>(T) from T to T {}

@:using(unit.issues.Issue9681)
private abstract Bar<T>(T) from T to T {}
