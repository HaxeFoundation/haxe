package unit.issues;

class Issue11800 extends unit.Test {
	public function test() {
		static var a = 0; // Works.
		var buf = new StringBuf();
		function append(v:Int) {
			buf.add(Std.string(v));
		}
		for (i in 0...3) {
			switch i {
				case n if (n < 2):
					append(++a);
					static var b = 0; // Not static.
					append(++b); // Always `1`.
				case _:
			}
		}
		eq("1122", buf.toString());
	}
}
