package cases.display;

class CompletionKeepsPositions extends TestCase {
	function test(_) {
		vfs.putContent("Main.hx", "class Main {
	static function main() {}
}");
		var init = "#if macro
import haxe.macro.Context;
import haxe.macro.Expr;

class Init {
	public static function run() {
		var x:Int = \"init\";
	}
}
#end";
		vfs.putContent("Init.hx", init);
		var args = ["-main", "Main", "--interp", "--macro", "Init.run()"];
		runHaxe(args);
		assertErrorMessage("String should be Int");
		Assert.isTrue(lastResult.stderr.indexOf("Init.hx:7:") != -1, lastResult.stderr);
		runHaxeJson(args, DisplayMethods.Completion, {file: new FsPath("Init.hx"), offset: init.indexOf("macro.Expr"), wasAutoTriggered: false});
		runHaxe(args);
		assertErrorMessage("String should be Int");
		Assert.isTrue(lastResult.stderr.indexOf("Init.hx:7:") != -1, lastResult.stderr);
	}
}
