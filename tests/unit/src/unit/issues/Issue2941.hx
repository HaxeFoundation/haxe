package unit.issues;

class Issue2941 extends Test {
	public function test()
	{
		var a = new Vector<Float>(10);
		eq(a.length,10);
	}
}

private typedef Vector<T> = haxe.ds.Vector<T>;

