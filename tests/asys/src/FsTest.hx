import haxe.io.Bytes;
import haxe.PosInfos;

using StringTools;

/**
	Base class for filesystem-related tests
**/
class FsTest extends Test {

	function setup() {
		var tempDir = 'test-data/temp';
		//TODO: Find a way to create & cleanup `test-data/temp` directory without using old sys API
		if(!sys.FileSystem.exists(tempDir))
			sys.FileSystem.createDirectory(tempDir);
		switch sys.FileSystem.readDirectory(tempDir) {
			case []:
			case _:
				if(isWindows)
					Sys.command('rmdir', [tempDir, '/S', '/Q'])
				else
					Sys.command('rm', ['-rf', tempDir]);
				sys.FileSystem.createDirectory(tempDir);
		}
	}

	/**
	 * Expected content of `test-data/bytes.bin` file
	 */
	function bytesBinContent():Bytes {
		var data = Bytes.alloc(256);
		for(i in 0...data.length) data.set(i, i);
		return data;
	}

	/**
		"Slash-insensitive" comparison of two strings representing file paths.
		E.g. `equalPaths("path/to/file", "path\\to\\file");` passes on windows
		(but still fails on other systems)
	**/
	function equalPaths(expected:String, actual:String, ?msg:String, ?pos:PosInfos) {
		if(isWindows) {
			msg = msg == null ? 'expected path "$expected" but it is "$actual"' : msg;
			equals(expected.replace('/', '\\'), actual.replace('/', '\\'), msg, pos);
		} else {
			equals(expected, actual, msg, pos);
		}
	}
}