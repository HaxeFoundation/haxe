package unit.issues;

class Issue5160 extends unit.Test {
	function test() {
        var bytes = haxe.io.Bytes.alloc(256);
        bytes.setInt32(0, -1241956892);
		eq(-1241956892, bytes.getInt32(0));
	}
}