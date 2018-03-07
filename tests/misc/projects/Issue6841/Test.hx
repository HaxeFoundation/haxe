class Test {
	static function error(msg, code) {
		Sys.stderr().writeString(msg);
		Sys.exit(code);
	}

	static function main() {
		var tests = [
			"Main1.hx" => "aaa",
			"Main2.hx" => "[a,a,a]",
			"Main3.hx" => "[a,a,a]",
			"Main4.hx" => "[a,a,a]",
			"Main5.hx" => "[a,a,a]",
			"Main6.hx" => "{2 => a, 1 => a}",
		];
		for (key in tests.keys()) {
			var proc = new sys.io.Process("haxe", ["-dce", "full", "-x", key]);
			var stderr = proc.stderr.readAll().toString();
			var exit = proc.exitCode();
			if (exit != 0) {
				error(stderr, exit);
			} else {
				var stdout = proc.stdout.readAll().toString();
				if (stdout.indexOf(tests[key]) == -1)
					error('toString NOT kept in ${key}\n', 1);
			}
		}
	}
}
