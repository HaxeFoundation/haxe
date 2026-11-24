package cases.display;

class StaticInitDisplay extends TestCase {
	function test(_) {
		var init = "class Init {
	static function __init__() {
		var s = \"a\";
		for (i in 0...3) s += i;
		trace(s);
	}
	public static var v = 1;
}";
		vfs.putContent("Init.hx", init);
		vfs.putContent("Dep.hx", "class Dep { public static var d = 1; }");
		vfs.putContent("Main.hx", "class Main {
	static function main() {
		trace(Init.v + Dep.d);
	}
}");
		var args = ["-main", "Main", "--interp"];
		runHaxe(args);
		assertSuccess();
		var hover = runHaxeJson(args, DisplayMethods.Hover, {file: new FsPath("Init.hx"), offset: init.indexOf("trace(s)") + 7});
		Assert.notNull(hover);
		runHaxeJson(args, DisplayMethods.Hover, {file: new FsPath("Init.hx"), offset: init.indexOf("var s")});
		vfs.putContent("Dep.hx", "class Dep { public static var d = 2; }");
		runHaxe(args);
		assertSuccess();
		assertHasPrint("Init.hx:5: a012");
		assertHasPrint("Main.hx:3: 3");
	}
}
