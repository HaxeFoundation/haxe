package unit.issues;

class Issue3607 extends Test {
  function test() {
    var foo = new Foo();
    eq(1, foo.one());

    var bar = new Bar();
    eq(-1, bar.one());
  }

  public static function one(foo: Foo) {
    return 1;
  }
}

@:using(unit.issues.Issue3607)
private class Foo {
  public function new() {}
}

private class Bar extends Foo {
  public function new() { super(); }

  public function one() {
    return super.one() - 2;
  }
}
