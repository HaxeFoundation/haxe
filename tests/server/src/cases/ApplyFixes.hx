package cases;

class ApplyFixes extends TestCase {
	function testCachedModuleFixes(_) {
		vfs.putContent("Old.hx", "class Old {\n\t@:deprecated(\"use now\")\n\t@:deprecated.replace(Old.now(x))\n\tpublic static function before(x:Int) return x;\n\n\tpublic static function now(x:Int) return x;\n}");
		vfs.putContent("Main.hx", "class Main {\n\tstatic function main() {\n\t\tOld.before(1);\n\t}\n}");
		var args = ["--main", "Main", "--fix-diff", "--no-output"];
		runHaxe(args);
		Assert.contains("+\t\tOld.now(1);", lastResult.prints);
		runHaxe(args);
		assertReuse("Main");
		// Fixes come from the module cache, like the warnings.
		Assert.contains("+\t\tOld.now(1);", lastResult.prints);
		runHaxe(args.concat(["-w", "-WDeprecated"]));
		Assert.notContains("+\t\tOld.now(1);", lastResult.prints);
	}
}
