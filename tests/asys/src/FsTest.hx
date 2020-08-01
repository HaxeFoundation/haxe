import haxe.io.Bytes;

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
}