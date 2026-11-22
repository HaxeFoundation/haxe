package cases.display;

class CoroutineDisplay extends TestCase {
	static final coro = "class Coro {
	@:coroutine public static function add(a:Int):Int {
		return a + 1;
	}

	@:coroutine public static function chain():String {
		var x = add(1);
		return \"chain:\" + x;
	}

	public static function run() {
		var f:haxe.coro.Coroutine<() -> String> = () -> chain();
		var g = f;
	}
}";

	@:coroutine function compile(args:Array<String>) {
		runHaxe(args);
		assertSuccess();
		return sys.io.File.getContent(haxe.io.Path.join([testDir, "bin/main.js"]));
	}

	function setupCoro() {
		vfs.putContent("Coro.hx", coro);
		vfs.putContent("Main.hx", "class Main { static function main() { Coro.run(); } }");
		return ["-main", "Main", "-js", "bin/main.js"];
	}

	function testSignatureHelp(_) {
		var args = setupCoro();
		var output = compile(args);
		for (autoTriggered in [true, false]) {
			runHaxeJson(args, DisplayMethods.SignatureHelp, {file: new FsPath("Coro.hx"), offset: coro.indexOf("chain():String") + 2, wasAutoTriggered: autoTriggered});
			Assert.equals(output, compile(args));
		}
	}

	function testHover(_) {
		var args = setupCoro();
		var output = compile(args);
		for (offset in [coro.indexOf("public static function chain") + 1, coro.indexOf("chain():String") + 1]) {
			runHaxeJson(args, DisplayMethods.Hover, {file: new FsPath("Coro.hx"), offset: offset});
			Assert.equals(output, compile(args));
		}
	}

	function testToplevelCompletion(_) {
		var args = setupCoro();
		var output = compile(args);
		runHaxeJson(args, DisplayMethods.Completion, {file: new FsPath("Coro.hx"), offset: coro.indexOf("f;"), wasAutoTriggered: false});
		Assert.equals(output, compile(args));
		runHaxeJson(args, DisplayMethods.Completion, {file: new FsPath("Coro.hx"), offset: coro.indexOf("f;"), wasAutoTriggered: false});
		Assert.equals(output, compile(args));
	}
}
