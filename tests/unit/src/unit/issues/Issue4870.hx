package unit.issues;

class Issue4870 extends Test {
  @:keep static inline var VALUE:UInt = 0x9747b28c;
  @:keep static var VALUE2:UInt = 0x9747b28c;
  function test() {
    t(VALUE == 0x9747b28c);
    t(VALUE2 > 0);
  }
}

