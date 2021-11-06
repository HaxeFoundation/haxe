import asys.native.filesystem.FilePath;
import haxe.io.Bytes;
import haxe.PosInfos;

using haxe.io.Path;
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
	function equalPaths(expected:Null<String>, actual:Null<String>, ?msg:String, ?pos:PosInfos) {
		if(expected == null || actual == null) {
			equals(expected, actual, msg, pos);
		} else {
			if(isWindows) {
				expected = expected.replace('/', '\\');
				actual = actual.replace('/', '\\');
			}
			if(expected != actual) {
				expected = removeTrailingSlashes(expected);
				actual = removeTrailingSlashes(actual);
			}
			equals(expected, actual, msg, pos);
		}
	}

	static final driveOnly = ~/^[a-zA-Z]:$/;

	function removeTrailingSlashes(path:String):String {
		var trimmed = Path.removeTrailingSlashes(path);
		if(isWindows && driveOnly.match(trimmed) && path != trimmed)
			trimmed = path.substr(0, 3);
		return trimmed;
	}
}