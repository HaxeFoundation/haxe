package unit.issues;
import haxe.ds.Option;

class Issue9789 extends Test {
  function test() {
    eq(1,switchFallback(Some(1),2));
    eq(2,switchFallback(None,2));

    eq(11,switchTuples(Some(1),Some(1)));
    eq(10,switchTuples(Some(2),Some(3)));
    eq(12,switchTuples(None,Some(3)));
    eq(33,switchTuples(Some(33),Some(1)));
    eq(-1,switchTuples(None,Some(1)));

    eq(14,swithFoo(Two(12,2)));
    eq(54,swithFoo(One(55)));
    eq(18,swithFoo(Zero));
  }

  function switchFallback(option: Option<Int>, fallback: Int) {
    return switch (option) {
      case
        Some(int) |
        None.with(int=fallback)
      :
        int;
    };
  }

  function switchTuples(optionA: Option<Int>, optionB: Option<Int>) {
    return switch [optionA, optionB] {
      case
        [Some(1),Some(1)].with(key=11) |
        [Some(2.with(key=10)),_] |
        [_,(_ => Some(3)).with(key=12)] |
        [Some(key),_] |
        [_,_].with(key=-1)
      :
        key;
    };
  }

  function swithFoo(foo: Foo) {
    return switch (foo) {
      case
        Two(x,y) |
        One(x).with(y=-1) |
        Zero.with(x=9,y=9)
      :
        x + y;
    }
  }
}

private enum Foo {
  Zero;
  One(x: Int);
  Two(x: Int, y: Int);
}
