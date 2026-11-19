import haxe.io.Path;
using StringTools;
import sys.FileSystem;
import sys.io.File;
import sys.io.Process;

class Macro {
	static function init() {
		var cwd = Sys.getCwd();
		Sys.setCwd("sub");
		check(Path.removeTrailingSlashes(Sys.getCwd()).endsWith("sub"), "Sys.getCwd");
		check(File.getContent("data.txt") == "sub", "File.getContent");
		File.saveContent("written.txt", "x");
		check(FileSystem.exists(Path.join([cwd, "sub", "written.txt"])), "File.saveContent");
		FileSystem.deleteFile("written.txt");
		var windows = Sys.systemName() == "Windows";
		var p = new Process(windows ? "cmd" : "cat", windows ? ["/c", "type", "data.txt"] : ["data.txt"]);
		var out = p.stdout.readAll().toString();
		p.close();
		check(out.indexOf("sub") >= 0, "Process");
	}

	static function check(b:Bool, what:String) {
		if (!b)
			throw what + " does not use the working directory set by the macro";
	}
}
