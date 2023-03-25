package unit.issues;
#if (cpp && !cppia)

class Issue9542 extends unit.Test {
  @:analyzer(no_optimize)
  function test() {
    var foo = 0;
    var bar: Bar = foo;
    foo += 1;
    eq(bar, 1);
  }
}

private abstract Bar(cpp.Reference<Int>) from cpp.Reference<Int> {}

#else
class Issue9542 extends unit.Test {}
#end
