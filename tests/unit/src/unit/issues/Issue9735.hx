package unit.issues;

class Issue9735 extends Test {
  function test() {
    var array = new Foo<Array<Int>>();
    eq(0, array.foo());

    var regexp = new Foo<EReg>("ereg", "");
    eq(0, regexp.foo());

    var string = new Foo<Foo<String>>("string");
    eq("string", string.t().t());

    var bar = new Foo<Bar>(2);
    eq(4, bar.t());
  }
}

@:forward.new private abstract Foo<T>(T) to T {
  public function foo() return 0;
  public function t() return this;
}

private abstract Bar(Int) to Int {
  public inline function new(int: Int) this = int * int;
}
