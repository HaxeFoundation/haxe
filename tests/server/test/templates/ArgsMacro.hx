class ArgsMacro {
	macro static public function test() {
		for (arg in Sys.args()) {
			if (arg.indexOf("arg_marker=") == 0) {
				Sys.println(arg);
				return macro null;
			}
		}
		throw "Missing arg_marker";
	}
}
