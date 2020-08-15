package unit.issues;
#if (cpp && !cppia)

class Issue9545 extends unit.Test {
  function test() {
    var first = new Vector<Bool>();
    first.push_back(true);
    eq(first.empty(), false);

    var second = new BoolVector();
    second.push_back(true);
    eq(second.empty(), false);

    var third = new Vector<Bool>(second);
    eq(third.empty(), false);

    var fourth = new GenVector<Bool>();
    fourth.push_back(true);
    eq(fourth.empty(), false);
  }
}

@:include('vector')
@:native('std::vector')
@:structAccess
private extern class Vector<T> {
  @:overload(function(other: cpp.Reference<Vector<T>>): Void {})
  function new(): Void;

  function push_back(value: T): Void;
  function empty(): Bool;
}

@:nativeGen
@:structAccess
private class BoolVector extends Vector<Bool> {
  public function new() {
    super();
  }
}

@:nativeGen
@:structAccess
private class GenVector<T> extends Vector<T> {
  public function new() {
    super();
  }
}

#else
class Issue9545 extends unit.Test {}
#end
