package cases.display;

class CoreDocDisplay extends TestCase {
	@:coroutine function compile(args:Array<String>) {
		runHaxe(args);
		assertSuccess();
		return sys.io.File.getContent(haxe.io.Path.join([testDir, "bin/php/lib/haxe/Exception.php"]));
	}

	function test(_) {
		var main = "class Main {
	static function main() {
		try {
			throw new haxe.Exception(\"x\");
		} catch (e:haxe.Exception) {
			trace(e.message);
		}
	}
}";
		vfs.putContent("Main.hx", main);
		var args = ["-main", "Main", "-php", "bin/php"];
		var output = compile(args);
		var hover = runHaxeJson(args, DisplayMethods.Hover, {file: new FsPath("Main.hx"), offset: main.indexOf("message") + 1});
		Assert.notNull(hover);
		Assert.equals(output, compile(args));
	}
}
