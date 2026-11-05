import sys.io.File;

class Check {
	static function main() {
		var actual = File.getContent("bin/Main.hx").split("\r\n").join("\n");
		if (actual != File.getContent("Main.hx.expected").split("\r\n").join("\n")) {
			Sys.println(actual);
			Sys.exit(1);
		}
	}
}
