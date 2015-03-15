package unit.issues;

class Issue2963 extends Test {
	public function test()
	{
		var a = new A<Int>();
		var v = a.get("k");
		eq(v,null);
		var x:Dynamic<Null<Int>> = {};
		var v2:Null<Int> = Reflect.field(x,'k');
		eq(v2,null);
		var v3:Null<Int> = Reflect.field(a,'k');
		eq(v3,null);
		var v4 = x.k;
		eq(v4,null);
	}
}

private abstract A<T>(Dynamic<Null<T>>) from Dynamic<Null<T>> {
    public inline function new() this = {}
    public inline function get(k:String):Null<T> return Reflect.field(this, k);
}
