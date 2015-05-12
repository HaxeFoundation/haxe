package unit.issues;

class Issue4000 extends Test
{
	public function test()
	{
		var c = new Child();
		eq(c.i,1000);
		eq(c.f,1.0);
		eq(c.dyn.a,1);
		eq(c.dyn.b,10);
		f(c.b);
#if (java || cs)
		var c = new Child(42);
		eq(c.i,42);
		eq(c.f,1.0);
		eq(c.dyn.a,1);
		eq(c.dyn.b,10);
		t(c.b);
#end
	}
}

#if !cpp @:nativeGen #end private class NativeGen
{
	public function new()
	{
	}
}

private class Child extends NativeGen
{
	public var i = 1000;
	public var f = 1.0;
	public var dyn = { a:1, b:10 };
	public var b = true;

#if (java || cs)
	@:overload
#end
	public function new()
	{
		super();
		this.b = false;
	}

#if (java || cs)
	@:overload public function new(i:Float)
	{
		super();
		this.i = Std.int(i);
	}
#end
}
