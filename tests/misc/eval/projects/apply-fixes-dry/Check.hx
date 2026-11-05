import sys.io.File;

class Check {
	static function main() {
		if (File.getContent("bin/Main.hx") != File.getContent("Main.hx.txt")) {
			Sys.exit(1);
		}
	}
}
