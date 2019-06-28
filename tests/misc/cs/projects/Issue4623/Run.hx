class Run {
	static public function run() {
		var exitCode = switch (Sys.systemName()) {
			case 'Windows':
				Sys.command('bin\\bin\\Main.exe');
			case _:
				Sys.command('mono', ['bin/bin/Main.exe']);
		}
		Sys.exit(exitCode);
	}
}