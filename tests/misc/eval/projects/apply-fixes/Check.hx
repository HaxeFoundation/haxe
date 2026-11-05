import sys.io.File;

class Check {
	static function read(path:String) {
		return File.getContent(path).split("\r\n").join("\n");
	}

	static function main() {
		var main = read("bin/src/Main.hx");
		if (main != read("Main.hx.expected")) {
			Sys.println(main);
			Sys.exit(1);
		}
		// Library class paths are only changed when selected
		if (read("lib/Old.hx").indexOf("Std.is(") == -1 || read("bin/other/Other.hx").indexOf("Std.isOfType(1, Int)") == -1) {
			Sys.exit(1);
		}
	}
}
