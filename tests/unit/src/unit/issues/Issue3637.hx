package unit.issues;

private abstract Abs(String) to String {
	inline public function new(s:String) this = s;
	public function raw() return this;
}

class Issue3637 extends Test {
	function test() {
		var map = new Map<Abs, Int>();
		map[new Abs("a")] = 1;
		map[new Abs("b")] = 1;

		var a:Array<Dynamic> = [];
		for (key in map.keys()) {
			a.push(key);
			a.push(map[key]);
		}
		eq(4, a.length);
		// TODO: some separate issue
		#if !cs
		t(Lambda.has(a, "a"));
		t(Lambda.has(a, "b"));
		t(Lambda.has(a, 1));
		t(a.remove(1));
		t(Lambda.has(a, 1));
		#end
	}
}