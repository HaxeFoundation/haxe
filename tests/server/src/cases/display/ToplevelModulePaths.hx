package cases.display;

class ToplevelModulePaths extends TestCase {
	function test(_) {
		vfs.putContent("Main.hx", "class Main {
	static function main() {
		A.f();
	}
}");
		vfs.putContent("A.hx", "class A {
	public static function f() {}
}");
		vfs.putContent("Other.hx", "class Other {
	static function g() {
		unknownIdent;
		A.f();
	}
}");
		var args = ["-main", "Main", "--interp"];
		runHaxe(args);
		assertSuccess();
		vfs.putContent("A.hx", "class A {
	public static function f() {}
	static var x:Int = \"s\";
}");
		runHaxeJson(args, DisplayMethods.Diagnostics, {file: new FsPath("Other.hx")});
		runHaxe(args);
		assertErrorMessage("String should be Int");
		Assert.isTrue(lastResult.stderr.indexOf("cases") == -1, lastResult.stderr);
	}
}
