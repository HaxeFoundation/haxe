package cases;

class CoroutineHoistedFields extends TestCase {
	function coro(op:String) {
		return 'class Coro {
	@:coroutine static function id(a:Int):Int {
		return a;
	}

	@:coroutine public static function chain():Int {
		var a = id(1);
		var b = id(2);
		var c = id(3);
		var d = id(4);
		var e = id(5);
		var f = id(6);
		return a $op b $op c $op d $op e $op f;
	}
}';
	}

	@:coroutine function compile(args:Array<String>) {
		runHaxe(args);
		assertSuccess();
		return sys.io.File.getContent(haxe.io.Path.join([testDir, "bin/main.py"]));
	}

	function test(_) {
		vfs.putContent("Main.hx", "class Main { static function main() { var f:haxe.coro.Coroutine<() -> Int> = Coro.chain; trace(f); } }");
		var args = ["-main", "Main", "-python", "bin/main.py"];
		vfs.putContent("Coro.hx", coro("+"));
		var output = compile(args);
		for (op in ["-", "*", "+"]) {
			vfs.putContent("Coro.hx", coro(op));
			runHaxe(args);
			assertSuccess();
		}
		Assert.equals(output, compile(args));
	}
}
