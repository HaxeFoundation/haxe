import sys.FileSystem;
import sys.io.File;

class Setup {
	static function run() {
		FileSystem.createDirectory("bin/src");
		FileSystem.createDirectory("bin/other");
		File.saveContent("bin/src/Main.hx", File.getContent("Main.hx.txt"));
		File.saveContent("bin/other/Other.hx", File.getContent("Other.hx.txt"));
	}
}
