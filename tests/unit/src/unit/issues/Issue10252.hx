package unit.issues;

class Issue10252 extends Test {
	function test() {
		var v:Float = 9007199254740991;
		var buf = new haxe.io.BytesBuffer();
		buf.addDouble(v);
		var bytes = buf.getBytes();
		var input = new haxe.io.BytesInput(bytes);
		var f = input.readDouble();
		feq(f, v);
	}
}
