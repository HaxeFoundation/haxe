package unit.issues;
#if (cpp && !cppia)

class Issue9807 extends unit.Test {
  function test() {
    var first = new BoolVector();
    first.push_back(true);

    var second = new BoolVector(first);
    eq(second.empty(), false);

    var third = new BoolVectorChild0();
    third.push_back(true);

    var fourth = new BoolVectorChild1(third);
    eq(fourth.empty(), false);
  }
}

@:include('vector')
@:native('std::vector<bool>')
@:structAccess
private extern class BoolVector {
  @:overload(function(other: cpp.Reference<BoolVector>): Void {})
  function new(): Void;

  function push_back(bool: Bool): Void;
  function empty(): Bool;
}

@:nativeGen
@:structAccess
private class BoolVectorChild0 extends BoolVector {
  public function new() {
    super();
  }
}

@:nativeGen
@:structAccess
private class BoolVectorChild1 extends BoolVector {
  public function new(other: cpp.Reference<BoolVector>) {
    super(other);
  }
}

#else
class Issue9807 extends unit.Test {}
#end
