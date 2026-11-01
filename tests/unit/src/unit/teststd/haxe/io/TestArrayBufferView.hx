package unit.teststd.haxe.io;

class TestArrayBufferView extends unit.Test {
	public function test() {

		var b = new haxe.io.UInt8Array(5);

		// set
		for( i in 0...5 )
			b[i] = i + 1;

		var buf = b.view.buffer;

		eq(buf.get(0), 1);
		eq(buf.get(4), 5);

		var s = b.view.sub(2);
		eq(s.byteOffset, 2);
		eq(s.byteLength, 3);
		eq(s.buffer, buf);

		var b2 = haxe.io.UInt8Array.fromBytes(s.buffer, s.byteOffset, s.byteLength);
		eq(b2[0], 3);
		b2[0] = 4;
		eq(b[3], 4);

	}
}
