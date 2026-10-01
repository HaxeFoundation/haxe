package cases;

class RunCommandArgs extends TestCase {
	// quoted command arguments must reach the shell as is in server mode
	function testArgumentWithSpace(_) {
		vfs.putContent("Main.hx", "class Main { static function main() {} }");
		runHaxe(["-main", "Main", "-neko", "bin/sub dir/main.n", "-D", "use-nekoc"]);
		assertSuccess();
	}

	function testCmdWithQuotedArgument(_) {
		vfs.putContent("Main.hx", "class Main { static function main() {} }");
		runHaxe(["-main", "Main", "-js", "bin/sub dir/out.js", "--cmd", 'node "bin/sub dir/out.js"']);
		assertSuccess();
	}

	function testCmdQuotedArgumentsArrive(_) {
		vfs.putContent("Main.hx", "class Main { static function main() {} }");
		runHaxe(["-main", "Main", "--interp", "--cmd", 'node -e "console.log(process.argv.slice(1).join(\'|\'))" "a b" "c&d"']);
		assertSuccess();
		assertHasPrint("a b|c&d");
	}

	function testCmdExitCode(_) {
		vfs.putContent("Main.hx", "class Main { static function main() {} }");
		runHaxe(["-main", "Main", "--interp", "--cmd", 'node -e "process.exit(3)"']);
		Assert.isTrue(lastResult.hasError);
	}
}
