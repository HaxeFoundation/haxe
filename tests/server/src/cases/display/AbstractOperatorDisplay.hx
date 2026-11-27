package cases.display;

class AbstractOperatorDisplay extends TestCase {
	function test(_) {
		var meters = "abstract Meters(Float) from Float to Float {
	@:op(A + B) static function add(a:Meters, b:Meters):Meters return (a : Float) + (b : Float);
}";
		vfs.putContent("Meters.hx", meters);
		vfs.putContent("Dep.hx", "class Dep { public static var v = 1; }");
		vfs.putContent("Main.hx", "class Main {
	static function main() {
		var m:Meters = Dep.v;
		trace(m + (3 : Meters));
	}
}");
		var args = ["-main", "Main", "--interp"];
		runHaxe(args);
		assertSuccess();
		runHaxeJson(args, DisplayMethods.Hover, {file: new FsPath("Meters.hx"), offset: meters.indexOf("(a : Float)") + 2});
		runHaxeJson(args, DisplayMethods.GotoDefinition, {file: new FsPath("Meters.hx"), offset: meters.indexOf("(b : Float)") + 2});
		vfs.putContent("Dep.hx", "class Dep { public static var v = 2; }");
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Dep.hx")});
		runHaxe(args);
		assertSuccess();
		assertHasPrint("Main.hx:4: 5");
	}
}
