class Run {
	static public function run() {
		var exe = 'bin/bin/Main.exe';
		var exitCode = switch (Sys.systemName()) {
			case 'Windows':
				Sys.command(exe);
			case _:
				Sys.command('mono', [exe]);
		}
		Sys.exit(exitCode);
	}
}