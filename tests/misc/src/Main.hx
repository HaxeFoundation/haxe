import haxe.macro.Compiler;
import sys.FileSystem;
import sys.io.File;
import haxe.io.Path;

using StringTools;

typedef Result = {
	count:Int,
	failures:Int
}

class Main {
	static public function main() {
		var result:Result = compileProjects();
		Sys.println('Done running ${result.count} tests with ${result.failures} failures');
		Sys.exit(result.failures);
	}

	static public function compileProjects():Result {
		var count = 0;
		var failures = 0;
		var filter = Compiler.getDefine("MISC_TEST_FILTER");
		var filterRegex = filter == null ? ~/.*/ : new EReg(filter, "");
		function browse(dirPath) {
			var dir = FileSystem.readDirectory(dirPath);
			dir.sort(Reflect.compare);
			for (file in dir) {
				var path = Path.join([dirPath, file]);
				if (FileSystem.isDirectory(path)) {
					if (path.endsWith('.disabled')) {
						Sys.println('Skipping $path');
					} else {
						browse(path);
					}
				} else if (file.endsWith(".hxml") && !file.endsWith("-each.hxml") && filterRegex.match(path)) {
					var old = Sys.getCwd();
					Sys.setCwd(dirPath);
					Sys.println('Running haxe $path');
					var expectFailure = file.endsWith("-fail.hxml");
					var expectStderr = if (FileSystem.exists('$file.stderr')) prepareExpectedOutput(File.getContent('$file.stderr')) else null;
					var success = runCommand("haxe", [file], expectFailure, expectStderr);
					++count;
					if (!success) {
						failures++;
					}
					Sys.setCwd(old);
				}
			}
		}
		browse("projects");
		return {
			count: count,
			failures: failures
		}
	}

	static function prepareExpectedOutput(s:String):String {
		s = s.replace("\r\n", "\n"); // get rid of windows newlines

		var cwd = Path.removeTrailingSlashes(FileSystem.fullPath(Sys.getCwd()));

		var context = {cwd: cwd};
		var macros = {normPath: normPath};

		return new haxe.Template(s).execute(context, macros);
	}

	static function normPath(_, p:String, properCase = false):String {
		if (Sys.systemName() == "Windows") {
			// on windows, haxe returns lowercase paths with backslashes, drive letter uppercased
			p = p.substr(0, 1).toUpperCase() + p.substr(1);
			p = p.replace("/", "\\");
			if (!properCase)
				p = p.toLowerCase();
		}
		return p;
	}

	static function runCommand(command:String, args:Array<String>, expectFailure:Bool, expectStderr:String) {
		#if timeout
		switch Sys.systemName() {
			case 'Linux':
				args.unshift(command);
				args.unshift(Compiler.getDefine('timeout'));
				command = 'timeout';
			case _:
				throw 'Running tests with timeout is not implemented for this OS';
		}
		#end
		var proc = new sys.io.Process(command, args);
		var stdout = proc.stdout.readAll();
		var exit = proc.exitCode();
		var success = exit == 0;
		// 124 - exit code of linux `timeout` command in case it actually timed out
		if (exit == 124) {
			Sys.println('Timeout. No response in ${Compiler.getDefine('timeout')} seconds.');
		}
		var result = switch [success, expectFailure] {
			case [true, false]:
				true;
			case [true, true]:
				Sys.println("Expected failure, but no failure occurred");
				false;
			case [false, true]:
				true;
			case [false, false]:
				var stderr = proc.stderr.readAll().toString();
				Sys.print(stderr);
				false;
		}

		if (stdout.length > 0) {
			Sys.println(stdout);
		}

		if (result && expectStderr != null) {
			var stderr = proc.stderr.readAll().toString().replace("\r\n", "\n").trim();
			if (stderr != expectStderr.trim()) {
				Sys.println("Actual stderr output doesn't match the expected one");
				Sys.println('Expected:\n"$expectStderr"');
				Sys.println('Actual:\n"$stderr"');
				result = false;
			}
		}
		proc.close();
		return result;
	}
}
