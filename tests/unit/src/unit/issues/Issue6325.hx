package unit.issues;

class Issue6325 extends Test {
#if (!hl && !flash && !cpp)
  public function test() {
    var base = new Base();
    base.someInt = 42;
    var child = new Child();
    child.otherInt = 142;
    child.someInt = 242;

    var ubase = new UsesBase();
    var uchild = new UsesChild();

    eq(ubase.doSomething(child), 142);
    eq(uchild.doSomething(child), 242);
    eq(uchild.doSomethingBase(child), 243);
    eq(uchild.doSomething(( child : Base )), 242);
    eq(uchild.doSomethingBase(( child : Base )), 243);
    eq(uchild.doSomething(base), 42);
    eq(uchild.doSomethingBase(base), 43);
  }
}

private class Base {
  public var someInt:Int = 0;
  public function new() {}
}
private class Child extends Base {
  public var otherInt:Int = 0;
}

private class UsesBase {
  public function new() {
  }

  public function doSomething(child:Child) {
    return child.otherInt;
  }
}

private class UsesChild extends UsesBase {
  override public function doSomething(base:Base) {
    return base.someInt;
  }

  public function doSomethingBase(base:Base) {
    return doSomething(base) + 1;
  }
#end
}
