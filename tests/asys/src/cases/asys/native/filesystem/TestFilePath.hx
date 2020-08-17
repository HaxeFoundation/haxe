package cases.asys.native.filesystem;

import asys.native.filesystem.FsException;
import haxe.exceptions.EncodingException;
import haxe.io.Bytes;
import asys.native.filesystem.FilePath;

class TestFilePath extends FsTest {
	/**
	 * Allocates 255 bytes with values from 1 to 255.
	 */
	static function allBytes():Bytes {
		var b = Bytes.alloc(254);
		for (i in 0...b.length)
			b.set(i, i + 1);
		return b;
	}

	function testToString_nonUnicodePath_throwsEncodingException() {
		var p:FilePath = allBytes();
		raises(() -> (p:String), EncodingException);
	}

	function testFromBytes_toBytes() {
		var b = allBytes();
		var p:FilePath = b;
		equals(0, b.compare(p));
	}

	function testAbsolute() {
		inline function check(cases:Map<String,String>) {
			for(path => expected in cases)
				equals(expected, (path:FilePath).absolute().toString());
		}
		var cwd = Sys.getCwd();

		var cases = [
			'./' => haxe.io.Path.removeTrailingSlashes(cwd),
			'non-existent.file' => cwd + 'non-existent.file',
			'path/to/../../non-existent.file' => cwd + 'non-existent.file'
		];
		check(cases);
		cases = if(isWindows) {
			[
				'/absolute/path' => '\\absolute\\path',
				'C:\\absolute\\path' => 'C:\\absolute\\path'
			];
		} else {
			[
				'/absolute/path' => '/absolute/path'
			];
		}
		check(cases);
	}

	function specToReadableString() {
		var b = Bytes.ofString('xyz😂/éé');
		var p:FilePath = b;
		'xyz😂/éé' == p.toReadableString();

		b.set(1, 0xE9); //Replace "y" with an invalid code point
		var p:FilePath = b;
		'x?z😂/éé' == p.toReadableString();
		'x*z😂/éé' == p.toReadableString('*'.code);
	}

	function specFromString_toString() {
		var s = "𠜎/aa😂/éé";
		var p:FilePath = s;
		s == p.toString();
	}
}
