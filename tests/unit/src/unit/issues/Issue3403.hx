package unit.issues;

class Issue3403 extends Test  {
	function test() {
		var val = [1,2,3,4];
		var x = Std.instance(val, Array);
		eq(val,x);

		var val:Dynamic = val;
		x = Std.instance(val,Array);
		eq(val,x);
		t(val != null);
		t(x != null);

		var g = new G([1,2,3,4]);
		var g2:Dynamic = g;
		var g3:G<Array<Int>> = Std.instance(g2,G);
		eq(g,g2);
		eq(g,g3);
		t(g3 != null);
		t(g != null);

		g2 = "Incorrect type";
		g3 = Std.instance(g2,G);
		eq(g3,null);

		g2 = 10;
		g3 = Std.instance(g2,G);
		eq(g3,null);
	}
}


private class G<T>
{
	public var val(default,null):T;
	public function new(val)
	{
		this.val = val;
	}
}
