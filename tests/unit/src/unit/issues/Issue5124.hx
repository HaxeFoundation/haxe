package unit.issues;

class Issue5124 extends Test {
  static var dblValue = 1.665;
  function test()
  {
    run((1.5555:R));
    eq((1.5555:R), 1);
    run((dblValue:R));
    eq((dblValue:R), 1);
  }

  dynamic function run(t:Dynamic) {
    eq(t,1);
  }
}

private abstract R(Float) to Float {
    private inline function new(v:Float)
        this = Std.int(v);
    @:from
    public static inline function fromFloat(v:Float):R
        return new R(v);
}
