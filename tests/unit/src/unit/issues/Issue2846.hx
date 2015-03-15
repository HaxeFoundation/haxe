package unit.issues;

class Issue2846 extends Test {
	function test() {
		var bytes = haxe.io.Bytes.alloc(8);
		bytes.set(0,0x40);
		var input = new haxe.io.BytesInput(bytes);
		input.bigEndian=true;
		feq(2.0, input.readDouble());
	}
}