package cases.asys.native.filesystem;

import haxe.exceptions.EncodingException;
import haxe.io.Bytes;
import asys.native.filesystem.FilePath;

class TestFilePath extends Test {
	/**
	 * Allocates 255 bytes with values from 0 to 255.
	 */
	static function arbitraryBytes():Bytes {
		var b = Bytes.alloc(255);
		for (i in 0...b.length)
			b.set(i, i);
		return b;
	}

	function testToString_nonUnicodePath_throwsEncodingException() {
		var p:FilePath = arbitraryBytes();
		Assert.raises(() -> (p:String), EncodingException);
	}

	function testToReadableString() {
		var b = Bytes.ofString('xyz');
		var p:FilePath = b;
		Assert.equals('xyz', p.toReadableString());

		b.set(1, 0xE9); //Replace "y" with an invalid code point
		var p:FilePath = b;
		Assert.equals('x?z', p.toReadableString());
		Assert.equals('x*z', p.toReadableString('*'.code));
	}

	function testFromBytes_toBytes() {
		var b = arbitraryBytes();
		var p:FilePath = b;
		Assert.equals(0, b.compare(p));
	}

	function testFromString_toString() {
		var s = "𠜎/aa😂/éé";
		var p:FilePath = s;
		Assert.equals(s, (p:String));
	}
}
