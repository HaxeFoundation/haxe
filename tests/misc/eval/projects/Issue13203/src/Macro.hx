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
		var p = new Process(windows ? "type data.txt" : "cat data.txt");
		var out = p.stdout.readAll().toString();
		p.close();
		check(out.indexOf("sub") >= 0, "Process with a shell command");
		check(Sys.command(windows ? "findstr sub data.txt >nul" : "grep -q sub data.txt") == 0, "Sys.command");
		check(Sys.command(windows ? "findstr" : "grep", windows ? ["sub", "data.txt"] : ["-q", "sub", "data.txt"]) == 0, "Sys.command with arguments");
		// the process is the program itself, not a shell started for the working directory
		if (!windows) {
			var p = new Process("sh", ["-c", "echo $$"]);
			var out = p.stdout.readAll().toString().trim();
			ensure(p.exitCode() == 0 && out == Std.string(p.getPid()), "Process.getPid gives the pid of the program");
			p.close();
		}
		var p = new Process(windows ? "ping" : "sleep", windows ? ["-n", "60", "127.0.0.1"] : ["60"]);
		var start = Sys.time();
		p.kill();
		p.exitCode();
		p.close();
		ensure(Sys.time() - start < 30, "Process.kill stops the program");
	}

	static function check(b:Bool, what:String) {
		if (!b)
			throw what + " does not use the working directory set by the macro";
	}

	static function ensure(b:Bool, what:String) {
		if (!b)
			throw "expected: " + what;
	}
}
