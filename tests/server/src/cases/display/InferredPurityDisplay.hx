package cases.display;

class InferredPurityDisplay extends TestCase {
	function test(_) {
		var points = "class Points {
	public static function len2(x:Int, y:Int):Int return x * x + y * y;
}";
		vfs.putContent("Points.hx", points);
		vfs.putContent("Main.hx", "class Main {
	static function main() {
		trace(Points.len2(1, 2));
	}
}");
		var args = ["-main", "Main", "--interp", "-D", "analyzer-optimize"];
		runHaxe(args);
		assertSuccess();
		var offset = points.indexOf("public static");
		runHaxeJson(args, DisplayMethods.GotoDefinition, {file: new FsPath("Points.hx"), offset: offset});
		assertSuccess();
		var hover = runHaxeJson(args, DisplayMethods.Hover, {file: new FsPath("Points.hx"), offset: offset});
		Assert.isNull(hover);
	}
}
