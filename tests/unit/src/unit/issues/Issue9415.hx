package unit.issues;

class Issue9415 extends unit.Test {
	function test() {
		var b = haxe.io.Bytes.alloc(1);
		b.set(0, 255);
		b.set(0, b.get(0) + 1);
		eq(0, b.get(0));
	}
}