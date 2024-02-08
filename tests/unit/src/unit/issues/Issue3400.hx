package unit.issues;

class Issue3400 extends Test
{
#if jvm
	public function test()
	{
		var a:AbstractList<haxe.Int64> = AbstractList.empty();
		noAssert();
	}
#end
}

#if jvm
private abstract AbstractList<T>(java.util.ArrayList<T>)
{
	function new(a:T)
	{
		this = new java.util.ArrayList<T>();
		this.add(a);
	}
	public static inline function empty<T>() return new AbstractList<T>((cast null:T));
}
#end
