package unit.issues;

class Issue6520 extends Test
{
  function test()
  {
    eq(new C().foo(10), 10);
    eq(new D().foo("test"), "test");
    eq(new CC().foo(10), 10);
    eq(new DD().foo("test"), "test");
  }
}

@:keep private interface A<T> {
	public function foo (a:T):T;
}

@:keep private interface B<T> extends A<T> {
	public function bar (a:T):T;
}

@:keep private class C implements A<Int> implements B<Int> {
  public function new()
  {
  }
	public function bar (a:Int):Int { return a;};
	public function foo (a:Int):Int { return a;};
}

@:keep private class D implements A<String> implements B<String> {
  public function new()
  {
  }
	public function bar (a:String):String { return a;};
	public function foo (a:String):String { return a;};
}

@:keep private interface AA<T> {
	public function foo (a:T):T;
}

@:keep private interface BB<T> extends AA<T> {
	public function bar (a:T):T;
}

@:keep private class CC implements AA<Int> implements BB<Int> implements A<Int> implements B<Int> {
  public function new()
  {
  }
	public function bar (a:Int):Int { return a;};
	public function foo (a:Int):Int { return a;};
}

@:keep private class DD implements AA<String> implements BB<String> implements A<String> implements B<String> {
  public function new()
  {
  }
	public function bar (a:String):String { return a;};
	public function foo (a:String):String { return a;};
}