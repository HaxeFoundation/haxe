package cases.display;

class DiagnosticsAfterCompilation extends TestCase {
	function test(_) {
		vfs.putContent("Main.hx", "class Main {
	static function main() {
		#if js
		var a:Int = \"a\";
		#else
		var b:Int = \"b\";
		#end
	}
}");
		var args = ["-main", "Main", "-js", "main.js", "--no-output"];
		var expected = runHaxeJson(args, DisplayMethods.Diagnostics, {file: new FsPath("Main.hx")})[0].diagnostics.map(d -> d.range.start.line);
		Assert.same([3, 5], expected);
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Main.hx")});
		runHaxe(args);
		assertErrorMessage("String should be Int");
		Assert.same(expected, runHaxeJson(args, DisplayMethods.Diagnostics, {file: new FsPath("Main.hx")})[0].diagnostics.map(d -> d.range.start.line));
	}
}
