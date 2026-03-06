class Main {
	static function main() {
		var p = new sys.io.Process("totally_nonexistent_command_12345", ["-v"]);
		var stdout = p.stdout.readAll().toString();
		var stderr = p.stderr.readAll().toString();
		var code = p.exitCode();
		p.close();

		if (code == 0) {
			Sys.stderr().writeString("ERROR: Expected non-zero exit code for non-existent command, got 0\n");
			Sys.exit(1);
		}
	}
}
