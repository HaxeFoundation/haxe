package unit.issues;

class Issue10057 extends unit.Test {
#if (cpp && !cppia)
  function test() {
    var i = 1;
    var star = cpp.Native.addressOf(i);
    var ref = cpp.Native.star(star);
    ref = 2;
    eq(2, i);
  }
#end
}
