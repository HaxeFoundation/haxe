import sys.FileSystem;
import utest.Assert;
using StringTools;

class TestFileSystem extends utest.Test {
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

	public function setup() {
		removeDir(dir);
		FileSystem.createDirectory(dir);
	}

	public function teardown() {
		removeDir(dir);
	}

	function testReadDirectory():Void {
		for (tailingSlash in tailingSlashes) {
			Assert.equals(0, FileSystem.readDirectory(dir).length);
			for (name in FileNames.names) {
				var path = dir + name + tailingSlash;
				FileSystem.createDirectory(path);
				var files = FileSystem.readDirectory(path);
				Assert.equals(0, files.length);
			}
			var files = FileSystem.readDirectory(dir);
			for (name in FileNames.names) {
				Assert.isTrue(files.indexOf(name) > -1);
			}
			Assert.equals(FileNames.names.length, files.length);
			for (name in FileNames.names) {
				FileSystem.deleteDirectory(dir + name);
			}

			//read current directory
			Assert.isTrue(FileSystem.readDirectory("." + tailingSlash).indexOf("compile.hxml") > -1);
			//read parent directory
			Assert.isTrue(FileSystem.readDirectory(".." + tailingSlash).indexOf("sys") > -1);
			//read directory with complex path
			Assert.isTrue(FileSystem.readDirectory("../sys/./.." + tailingSlash).indexOf("sys") > -1);
		}
	}

	function testCreateDirectory():Void {
		for (tailingSlash in tailingSlashes) {
			Assert.equals(0, FileSystem.readDirectory(dir).length);
			for (name in FileNames.names) {
				FileSystem.createDirectory(dir + name + tailingSlash);
			}
			var files = FileSystem.readDirectory(dir);
			for (name in FileNames.names) {
				Assert.isTrue(files.indexOf(name) > -1);
			}
			Assert.equals(FileNames.names.length, files.length);
			for (name in FileNames.names) {
				FileSystem.deleteDirectory(dir + name);
			}

			//create deep directory
			var path = dir + "1/2/3" + tailingSlash;
			FileSystem.createDirectory(path);
			Assert.isTrue(FileSystem.isDirectory(path));

			//create directory in complex path
			var path = dir + "1/../1/./../complex" + tailingSlash;
			FileSystem.createDirectory(path);
			Assert.isTrue(FileSystem.readDirectory(dir).indexOf("complex") > -1);

			FileSystem.deleteDirectory(dir + "1/2/3");
			FileSystem.deleteDirectory(dir + "1/2");
			FileSystem.deleteDirectory(dir + "1");
			FileSystem.deleteDirectory(dir + "complex");
		}
	}

	function testRootExists() {
		Assert.isTrue(FileSystem.exists("/"));
		Assert.isTrue(FileSystem.stat("/") != null);
	}

	function testWindowsSpecialCases() {
		if (Sys.systemName() != "Windows" #if python || true #end) {
			Assert.isTrue(true);
			return;
		}
		var withSlash = "C:/";
		var withBackslash = "C:\\";
		var without = "C:";

		for (path in [withSlash, withBackslash, without]) {
			Assert.isTrue(FileSystem.exists(path));
			Assert.isTrue(FileSystem.isDirectory(path));
			Assert.isTrue(FileSystem.stat(path) != null);
			Assert.isTrue(FileSystem.readDirectory(path) != null);
		}
	}

	function testStatDirectory() {
		var stat = FileSystem.stat(dir);
		Assert.isTrue(stat != null);
	}

	function testCreateExistingDirectory() {
		var testDir = dir + "exists";
		FileSystem.createDirectory(testDir);
		FileSystem.createDirectory(testDir); // shouldn't throw
		Assert.isTrue(FileSystem.isDirectory(testDir));
	}

	function testAbsolutePath() {
		var paths = [
			{ input: "c:\\nadako",   expected: "c:\\nadako" },
			{ input: "nadako.js",    expected: haxe.io.Path.join([Sys.getCwd(), "nadako.js"]) },
			{ input: "./nadako.js",  expected: haxe.io.Path.join([Sys.getCwd(), "/./nadako.js"]) },
			{ input: "/nadako",      expected: "/nadako" }
		];
		for (path in paths) {
			Assert.equals(normPath(path.expected), normPath(FileSystem.absolutePath(path.input)));
		}
	}

	static function normPath(p:String, properCase = false):String {
		if (Sys.systemName() == "Windows")
		{
			// on windows, haxe returns lowercase paths with backslashes, drive letter uppercased
			p = p.substr(0, 1).toUpperCase() + p.substr(1);
			p = p.replace("/", "\\");
			if (!properCase)
				p = p.toLowerCase();
		}
		return p;
	}
}