package unit.issues;

private class Def {
	public var applied = 0;

	public function new() {}

	public function apply() {
		applied++;
	}
}

private class Holder {
	public var def(default, set):Def;

	public function new() {}

	public function destroy() {
		def = null;
	}

	inline function set_def(newDef:Def):Def {
		newDef.apply();
		return def = newDef;
	}
}

class Issue5074 extends unit.Test {
	static var run = false;

	function test() {
		var h = new Holder();
		var d = new Def();
		h.def = d;
		eq(1, d.applied);
		// Only has to compile: the analyzer used to fuse `null` into `null()->apply()` on cpp
		if (run) {
			h.destroy();
		}
		eq(d, h.def);
	}
}
