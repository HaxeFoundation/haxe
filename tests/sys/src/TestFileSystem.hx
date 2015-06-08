import sys.FileSystem;

class TestFileSystem extends haxe.unit.TestCase {
	/**
		Recursively remove a given directory.
	*/
	static function removeDir(dir:String):Void {
		if (FileSystem.exists(dir)) {
			for (item in FileSystem.readDirectory(dir)) {
				item = haxe.io.Path.join([dir, item]);
				if (FileSystem.isDirectory(item)) {
					removeDir(item);
				} else {
					FileSystem.deleteFile(item);
				}
			}
			FileSystem.deleteDirectory(dir);
		}
	}

	var dir = "temp/TestFileSystem/";
	var tailingSlashes = switch (Sys.systemName()) {
		case "Windows": ["", "/", "\\"];
		case _: ["", "/"];
	}

	override public function setup() {
		removeDir(dir);
		FileSystem.createDirectory(dir);
	}

	override public function tearDown() {
		removeDir(dir);
	}

	function testReadDirectory():Void {
		for (tailingSlash in tailingSlashes) {
			assertEquals(0, FileSystem.readDirectory(dir).length);
			for (name in FileNames.names) {
				var path = dir + name + tailingSlash;
				FileSystem.createDirectory(path);
				var files = FileSystem.readDirectory(path);
				assertEquals(0, files.length);
			}
			var files = FileSystem.readDirectory(dir);
			for (name in FileNames.names) {
				assertTrue(files.indexOf(name) > -1);
			}
			assertEquals(FileNames.names.length, files.length);
			for (name in FileNames.names) {
				FileSystem.deleteDirectory(dir + name);
			}

			//read current directory
			assertTrue(FileSystem.readDirectory("." + tailingSlash).indexOf("compile.hxml") > -1);
			//read parent directory
			assertTrue(FileSystem.readDirectory(".." + tailingSlash).indexOf("sys") > -1);
			//read directory with complex path
			assertTrue(FileSystem.readDirectory("../sys/./.." + tailingSlash).indexOf("sys") > -1);
		}
	}

	function testCreateDirectory():Void {
		for (tailingSlash in tailingSlashes) {
			assertEquals(0, FileSystem.readDirectory(dir).length);
			for (name in FileNames.names) {
				FileSystem.createDirectory(dir + name + tailingSlash);
			}
			var files = FileSystem.readDirectory(dir);
			for (name in FileNames.names) {
				assertTrue(files.indexOf(name) > -1);
			}
			assertEquals(FileNames.names.length, files.length);
			for (name in FileNames.names) {
				FileSystem.deleteDirectory(dir + name);
			}

			//create deep directory
			var path = dir + "1/2/3" + tailingSlash;
			FileSystem.createDirectory(path);
			assertTrue(FileSystem.isDirectory(path));

			//create directory in complex path
			var path = dir + "1/../1/./../complex" + tailingSlash;
			FileSystem.createDirectory(path);
			assertTrue(FileSystem.readDirectory(dir).indexOf("complex") > -1);

			FileSystem.deleteDirectory(dir + "1/2/3");
			FileSystem.deleteDirectory(dir + "1/2");
			FileSystem.deleteDirectory(dir + "1");
			FileSystem.deleteDirectory(dir + "complex");
		}
	}

	function testWindowsSpecialCases() {
		if (Sys.systemName() != "Windows") {
			assertTrue(true);
			return;
		}
		var withSlash = "C:/";
		var withBackslash = "C:\\";
		var without = "C:";

		for (path in [withSlash, withBackslash, without]) {
			assertTrue(FileSystem.exists(path));
			assertTrue(FileSystem.isDirectory(path));
			assertTrue(FileSystem.stat(path) != null);
			assertTrue(FileSystem.readDirectory(path) != null);
		}
	}
}