package unit.issues;
#if (cpp && !cppia)
import cpp.Pointer;
import cpp.Reference;
import cpp.Star;
import cpp.Struct;

class Issue9516 extends unit.Test {
  function test() {
    var justFoo = new Foo(1);
    var structFoo: Struct<Foo> = new Foo(2);
    var starFoo: Star<Foo> = new Foo(3);
    var referenceFoo: Reference<Foo> = starFoo;

    eq(1, justFoo.fun());
    eq(2, structFoo.fun());
    eq(3, starFoo.fun());
    eq(3, referenceFoo.fun());

    var justBar = new Bar(4);
    var structBar: Struct<Bar> = new Bar(5);
    var starBar: Star<Bar> = new Bar(6);
    var referenceBar: Reference<Bar> = starBar;

    eq(4, justBar.fun());
    eq(5, structBar.fun());
    eq(6, starBar.fun());
    eq(6, referenceBar.fun());

    var justBaz = new Baz(3, 4);
    var structBaz: Struct<Baz> = new Baz(2, 6);
    var starBaz: Star<Baz> = new Baz(5, 4);
    var referenceBaz: Reference<Baz> = starBaz;

    eq(7, justBaz.fun());
    eq(8, structBaz.fun());
    eq(9, starBaz.fun());
    eq(9, referenceBaz.fun());

    Pointer.fromStar(starFoo).destroy();
    Pointer.fromStar(starBar).destroy();
    Pointer.fromStar(starBaz).destroy();
  }
}

@:nativeGen
@:structAccess
private class Foo {
  var value:Int;

  public function new(value: Int) {
    this.value = value;
  }

  public function fun() return value;
}

@:native("::unit::issues::_Issue9516::Foo")
@:include("unit/issues/_Issue9516/Foo.h")
@:structAccess
private extern class Bar {
  function new(value: Int);
  function fun(): Int;
}

@:nativeGen
@:structAccess
private class Baz extends Bar {
  var delta:Int;

  public function new(value: Int, delta: Int) {
    this.delta = delta;
    super(value);
  }

  public override function fun() return super.fun() + delta;
}

#else
class Issue9516 extends unit.Test {}
#end
