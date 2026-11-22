package cases.display;

class SignatureHelpCachedModule extends TestCase {
	function test(_) {
		var main = "class Main {
	static function main() {
		var b = new StringBuf();
		b.add(\"x\");
		Std.parseInt(\"1\");
	}
}";
		vfs.putContent("Main.hx", main);
		var args = ["-main", "Main", "--interp"];
		runHaxe(args);
		assertSuccess();
		for (autoTriggered in [true, false]) {
			var sig = runHaxeJson(args, DisplayMethods.SignatureHelp, {file: new FsPath("Main.hx"), offset: main.indexOf("\"x\""), wasAutoTriggered: autoTriggered});
			Assert.notNull(sig);
			Assert.equals("x", sig.signatures[0].args[0].name);
			var sig = runHaxeJson(args, DisplayMethods.SignatureHelp, {file: new FsPath("Main.hx"), offset: main.indexOf("\"1\""), wasAutoTriggered: autoTriggered});
			Assert.notNull(sig);
			Assert.equals("x", sig.signatures[0].args[0].name);
		}
	}

	function testDependencyChanged(_) {
		var main = "class Main {
	static function main() {
		Dep.f(\"x\");
	}
}";
		vfs.putContent("Main.hx", main);
		vfs.putContent("Dep.hx", "class Dep { public static function f(x:String) {} }");
		var args = ["-main", "Main", "--interp"];
		runHaxe(args);
		assertSuccess();
		vfs.putContent("Dep.hx", "class Dep { public static function f(x:String) { trace(1); } }");
		var sig = runHaxeJson(args, DisplayMethods.SignatureHelp, {file: new FsPath("Main.hx"), offset: main.indexOf("\"x\""), wasAutoTriggered: true});
		Assert.notNull(sig);
		Assert.equals("x", sig.signatures[0].args[0].name);
	}
}
