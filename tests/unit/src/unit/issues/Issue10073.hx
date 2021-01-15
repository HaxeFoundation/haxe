package unit.issues;

private abstract Foo(Array<Int>) from Array<Int> {
  @:op([])
  function get(index: Int): Int;

  @:op([])
  function set(index: Int, value: Int): Void;
}

#if eval
abstract Bar(Int) from Int {
  @:op(_ + _)
  extern function add(other: Int): Int;

  @:op([])
  extern function get(index: Int): Int;

  @:native('add')
  function doAdd(other: Int): Int
    return 39;

  @:native('get')
  function doGet(index: Int)
    return (this + index) * 2;
}
#end

class Issue10073 extends Test {
  function test() {
    var foo: Foo = [];
    foo[0] = 3;
    eq(3, foo[0]);

    #if eval
    var bar: Bar = 71;
    eq(39, bar + 1);
    eq(144, bar[1]);
    #end
  }
}
