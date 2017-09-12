package unit.issues;

class Issue5399 extends Test {
#if cs
  public function test() {
    var s:Something = null;
    s.width = 10;
    eq(s.width, 10);
  }
#end
}

#if cs
@:keep @:struct private class Something {
  public var width:Float;
  public function new() {
  }
}
#end
