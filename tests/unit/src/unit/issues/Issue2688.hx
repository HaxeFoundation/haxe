package unit.issues;

class Issue2688 extends Test {
#if (java || cs)
  public function test() {
    var x = 0;
    var b = new B(function() {
      x++;
    });
    b.tasks(10);
    eq(x, 10);
  }
#end
}

#if (java || cs)
@:nativeGen
private class A {
  public function new() {
  }
}

@:nativeGen
private class B extends A {
  public var tasks:Int->Void;

  public function new(task:()->Void) {
    super();
    tasks = function(i) {
      for (j in 0...i) {
        task();
      }
    }
  }
}
#end
