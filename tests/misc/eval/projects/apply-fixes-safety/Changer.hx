import haxe.macro.Context;
import sys.FileSystem;
import sys.io.File;

class Changer {
	static function setup() {
		FileSystem.createDirectory("bin");
		File.saveContent("bin/Changed.hx", "class Changed {\n\tstatic function main() {\n\t\tStd.is(1, Int);\n\t}\n}\n");
	}

	// Edits the source after typing, as an editor could during the compilation.
	static function init() {
		var done = false;
		Context.onAfterTyping(_ -> {
			if (!done) {
				done = true;
				File.saveContent("bin/Changed.hx", "\n" + File.getContent("bin/Changed.hx"));
			}
		});
	}
}
