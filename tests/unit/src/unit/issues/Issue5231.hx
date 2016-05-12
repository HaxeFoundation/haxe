package unit.issues;
using unit.issues.Issue5231;

class Issue5231 extends Test {
  public function test() {
    t(getTest().doesItWork(getTest()));
  }

  public static function getTest():Test1 {
    return cast {};
  }
}

private abstract Test2(Dynamic) {
}

@:forward
private abstract Test1(Test2) {
}

class I5231_Using {
  public static function doesItWork(t:Test1, t2:Test1):Bool {
    return true;
  }
}
