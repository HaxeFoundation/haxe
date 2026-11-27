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

	function testCompletion(_) {
		vfs.putContent("Points.hx", "class Points {
	public static function len2(x:Int, y:Int):Int return x * x + y * y;
}");
		var main = "class Main {
	static function main() {
		trace(Points.len2(1, 2));
	}
}";
		vfs.putContent("Main.hx", main);
		var args = ["-main", "Main", "--interp", "-D", "analyzer-optimize"];
		runHaxe(args);
		assertSuccess();
		var result = runHaxeJson(args, DisplayMethods.Completion, {file: new FsPath("Main.hx"), offset: main.indexOf("len2"), wasAutoTriggered: true});
		var len2 = result.items.filter(item -> item.kind == ClassField && item.args.field.name == "len2")[0];
		Assert.notNull(len2);
		Assert.same([], len2.args.field.meta.map(m -> m.name));
	}
}
