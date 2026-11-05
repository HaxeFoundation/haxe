import sys.io.File;

class Check {
	static function main() {
		var actual = File.getContent("bin/Main.hx").split("\r\n").join("\n");
		var expected = File.getContent("Main.hx.expected").split("\r\n").join("\n");
		if (actual != expected) {
			Sys.println(actual);
			Sys.exit(1);
		}
		// Fixes are only applied to user class paths
		if (File.getContent("lib/Old.hx").indexOf("Std.is(") == -1) {
			Sys.exit(1);
		}
	}
}
