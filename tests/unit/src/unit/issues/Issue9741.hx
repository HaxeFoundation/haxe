package unit.issues;

class Issue9741 extends Test {
  function test() {
    var any: Any = null;
    checkMainT(any);

    var arrayInt: Array<Int> = [0, 1];
    checkArrayAny(arrayInt);

    var arrayAny: Array<Any> = arrayInt;
    checkSameT(arrayInt, arrayInt);
    checkSameT(arrayAny, arrayAny);
    checkSameT(arrayAny, arrayInt);
    t(unit.HelperMacros.typeError(checkSameT(arrayInt, arrayAny)));

    t(unit.HelperMacros.typeError(((null: Bar): Int)));
    t(unit.HelperMacros.typeError(((null: Int): Bar)));
    ((null: {x: Bar}): {x: Int});
    ((null: {x: Int}): {x: Bar});
  }

  static function checkMainT<T:Issue9741>(v:T) {}

  static function checkArrayAny(a:Array<Any>) {}

  static function checkSameT<T>(a1:T, a2:T) {}
}

private class Foo {
  public function new() {}
}

@:forward.variance private abstract Bar(Int) {
  public inline function new() this = 0;
}
