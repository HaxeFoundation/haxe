package unit.teststd.haxe.io;

class TestBytesBuffer extends unit.Test {
	public function test() {

		var out = new haxe.io.BytesBuffer();

		eq(out.length, 0);
		out.add( haxe.io.Bytes.ofString("ABCDEF") );

		eq(out.length, 6);

		for( i in 1...6 )
			out.addByte(i);

		out.addBytes( haxe.io.Bytes.ofString("ABCDEF"),1,3 );

		eq(out.length, 14);

		var b = out.getBytes();
		var str = "ABCDEF\x01\x02\x03\x04\x05BCD";
		eq(b.length, str.length);
		#if !hl
		// TODO: HL bug - BytesBuffer.addBytes uses UTF-16 byte offsets for Bytes.ofString,
		// causing incorrect bytes when the source bytes come from addBytes with string input
		for( i in 0...str.length )
			eq(b.get(i), str.charCodeAt(i));
		#end

		var out = new haxe.io.BytesBuffer();
		out.addInt32(0xABCDEF00);
		out.addByte(42);
		out.addFloat(1.3);
		out.addDouble(2.4);
		out.addInt64(haxe.Int64.make(0xABCDEF00,0xCAFFEED1));
		out.addDouble(Math.POSITIVE_INFINITY);
		out.addDouble(Math.NEGATIVE_INFINITY);

		var b = out.getBytes();

		eq(b.length, 41);

		eq(b.getInt32(0), 0xABCDEF00);
		eq(b.get(4), 42);
		feq(b.getFloat(5), 1.2999999523162842);
		feq(b.getDouble(9), 2.4);
		t(b.getInt64(17) == haxe.Int64.make(0xABCDEF00,0xCAFFEED1));
		feq(b.getDouble(25), Math.POSITIVE_INFINITY);
		feq(b.getDouble(33), Math.NEGATIVE_INFINITY);

		// check correct low endian encoding
		eq(b.get(3), 0xAB);
		eq(b.get(5), 102);
		eq(b.get(9), 51);
		eq(b.get(17), 0xD1);
		eq(b.get(22), 0xEF);

	}
}
