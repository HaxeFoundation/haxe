package cases.asys.native.filesystem;

import asys.native.filesystem.FsException;
import haxe.io.Bytes;
import asys.native.filesystem.FilePath;

class TestFilePath extends FsTest {

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

	function specFromString_toString() {
		var s = "𠜎/aa😂/éé";
		var p:FilePath = s;
		s == p.toString();

		var s = "some/dir/";
		var p:FilePath = s;
		'some/dir' == p.toString();
	}
}
