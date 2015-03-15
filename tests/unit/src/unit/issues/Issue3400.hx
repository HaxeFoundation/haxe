package unit.issues;

class Issue3400 extends Test
{
#if java
	public function test()
	{
		var a:AbstractList<haxe.Int64> = AbstractList.empty();
	}
#end
}

#if java
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
