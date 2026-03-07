class Main {
	static function main() {
		// Creating a process for a non-existent command must not throw.
		// It should return a process with EOF stdout, the error in stderr,
		// and a non-zero exit code.
		var p = new sys.io.Process("totally_nonexistent_command_12345", ["-v"]);
		var stdout = p.stdout.readAll().toString();
		var stderr = p.stderr.readAll().toString();
		var code = p.exitCode();
		p.close();

		Sys.println('stdout_empty=${stdout.length == 0}');
		Sys.println('has_error_info=${stderr.length > 0}');
		Sys.println('exit_code=$code');
	}
}
