package unit.issues;

import haxe.ds.Option;

class Issue9577 extends Test {
  function test() {
    var foo: Foo = Some(9577);
    var bar: Bar = Some(9577);
    var baz: Baz = Some(9577);
    eq(true, foo.match(Some(9577)));
    eq(9578, bar.match(9577));
    eq(false, baz.match(None));
  }

  public static function match(baz: Baz, value: Int) {
    return value + 1;
  }
}

@:forward private abstract Foo(Option<Int>) from Option<Int> {}

@:forward private abstract Bar(Option<Int>) from Option<Int> {
  public function match(value: Int) {
    return value + 1;
  }
}

@:using(unit.issues.Issue9577)
@:forward private abstract Baz(Option<Int>) from Option<Int> {}
