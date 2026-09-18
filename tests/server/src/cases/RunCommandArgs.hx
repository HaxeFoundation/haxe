package cases;

class RunCommandArgs extends TestCase {
	// nekoc is invoked through run_command_args, which must not flatten its
	// arguments into a command string.
	function testArgumentWithSpace(_) {
		vfs.putContent("Main.hx", "class Main { static function main() {} }");
		runHaxe(["-main", "Main", "-neko", "bin/sub dir/main.n", "-D", "use-nekoc"]);
		assertSuccess();
	}
}
