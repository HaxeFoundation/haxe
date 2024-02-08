package unit.issues;

class Issue9498 extends unit.Test {
  #if (cpp && !cppia)
  function test() {
    eq(1, Foo.foo());
    eq(1, ExternFoo.foo());
  }
  #end
}

#if (cpp && !cppia)
@:nativeGen private class Foo {
  public static function foo() return 1;
}

@:native("::unit::issues::_Issue9498::Foo")
@:include("unit/issues/_Issue9498/Foo.h")
private extern class ExternFoo {
  static function foo(): Int;
}

@:nativeGen @:keep private class Bar extends ExternFoo {}
#end
