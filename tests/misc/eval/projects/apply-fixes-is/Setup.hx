import sys.FileSystem;
import sys.io.File;

class Setup {
	static function run() {
		FileSystem.createDirectory("bin");
		File.saveContent("bin/Main.hx", File.getContent("Main.hx.txt"));
	}
}
