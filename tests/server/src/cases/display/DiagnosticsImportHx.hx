package cases.display;

class DiagnosticsImportHx extends TestCase {
	function test(_) {
		vfs.putContent("pk/import.hx", "import pk.Defaults;");
		vfs.putContent("pk/Defaults.hx", "package pk;
class Defaults {
	public static var value = 1;
}");
		vfs.putContent("pk/Thing.hx", "package pk;
class Thing {
	public static function get() return Defaults.value;
}");
		vfs.putContent("Main.hx", "class Main {
	static function main() {
		trace(pk.Thing.get());
	}
}");
		var args = ["-main", "Main", "--interp"];
		runHaxe(args);
		assertSuccess();
		sys.FileSystem.deleteFile(testDir + "/pk/Defaults.hx");
		vfs.touch("pk");
		runHaxe(args);
		var expected = lastResult.stderr;
		Assert.isTrue(expected.indexOf("import.hx:1:") != -1, expected);

		vfs.putContent("pk/Defaults.hx", "package pk;
class Defaults {
	public static var value = 1;
}");
		runHaxe(args);
		assertSuccess();
		runHaxeJson(args, DisplayMethods.Diagnostics, {file: new FsPath("pk/import.hx")});
		sys.FileSystem.deleteFile(testDir + "/pk/Defaults.hx");
		vfs.touch("pk");
		runHaxe(args);
		Assert.equals(expected, lastResult.stderr);
	}
}
