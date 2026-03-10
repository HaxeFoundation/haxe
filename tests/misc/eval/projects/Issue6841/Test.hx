class Test {
	static function error(msg, code) {
		Sys.stderr().writeString(msg);
		Sys.exit(code);
	}

	static function main() {
		var proc = new sys.io.Process("haxe", ["-dce", "full", "-x", "Main.hx"]);
		var stderr = proc.stderr.readAll().toString();
		var exit = proc.exitCode();
		if (exit != 0) {
			error(stderr, exit);
		} else {
			var stdout = proc.stdout.readAll().toString();
			if (stdout.indexOf("aaa") == -1)
				error("toString NOT kept\n", 1);
		}
	}
}
