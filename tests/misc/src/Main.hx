import haxe.io.Bytes;
import haxe.macro.Compiler;
import sys.FileSystem;
import sys.io.File;
import haxe.io.Path;

using StringTools;

typedef Result = {
	count:Int,
	failures:Int,
	summary:String
}

class Main {
	static public function main() {
		var result:Result = compileProjects();
		Sys.println('Done running ${result.count} tests with ${result.failures} failures');
		if(result.count > 20 && result.failures > 0) {
			Sys.println('SUMMARY:');
			Sys.println(result.summary);
		}
		Sys.exit(result.failures);
	}

	static public function compileProjects():Result {
		var count = 0;
		var failures = 0;
		var failuresSummary = [];
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
					var expectStdout = if (FileSystem.exists('$file.stdout')) prepareExpectedOutput(File.getContent('$file.stdout')) else null;
					var expectStderr = if (FileSystem.exists('$file.stderr')) prepareExpectedOutput(File.getContent('$file.stderr')) else null;
					var result = runCommand("haxe", [file], expectFailure, expectStdout, expectStderr);
					++count;
					if (!result.success) {
						failures++;
						failuresSummary.push(path + '\n' + result.summary);
					}
					Sys.setCwd(old);
				}
			}
		}
		browse("projects");
		return {
			count: count,
			failures: failures,
			summary: failuresSummary.join('\n')
		}
	}

	static function prepareExpectedOutput(s:String):String {
		s = s.replace("\r\n", "\n"); // get rid of windows newlines

		var cwd = Path.removeTrailingSlashes(FileSystem.fullPath(Sys.getCwd()));
		var std = Path.removeTrailingSlashes(getStd());

		var context = {cwd: cwd, std: std};
		var macros = {normPath: normPath};

		return new haxe.Template(s).execute(context, macros);
	}

	static function normPath(_, p:String):String {
		if (Sys.systemName() == "Windows") {
			// on windows, haxe returns lowercase paths with backslashes, drive letter uppercased
			p = p.substr(0, 1).toUpperCase() + p.substr(1);
			p = p.replace("/", "\\");
		}
		return p;
	}

	static function runCommand(command:String, args:Array<String>, expectFailure:Bool, expectStdout:String, expectStderr:String):{success:Bool, summary:String} {
		var summary = [];
		function println(msg:String) {
			summary.push(msg);
			Sys.println(msg);
		}
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
			println('Timeout. No response in ${Compiler.getDefine('timeout')} seconds.');
		}
		var result = switch [success, expectFailure] {
			case [true, false]:
				true;
			case [true, true]:
				println("Expected failure, but no failure occurred");
				false;
			case [false, true]:
				true;
			case [false, false]:
				var stderr = proc.stderr.readAll().toString();
				Sys.print(stderr);
				false;
		}

		if (result && expectStdout != null) {
			result = checkOutput(println, "stdout", stdout.toString(), expectStdout);
		} else if (stdout.length > 0) {
			println(stdout.toString());
		}

		if (result && expectStderr != null) {
			result = checkOutput(println, "stderr", proc.stderr.readAll().toString(), expectStderr);
		}

		proc.close();
		return {success:result, summary:summary.join('\n')};
	}

	static function checkOutput(println:String->Void, name:String, content:String, expected:String):Bool {
		var content = content.replace("\r\n", "\n").trim();
		var expected = expected.trim();
		if (content != expected) {
			// "Picked up JAVA_TOOL_OPTIONS: <...>" is printed by JVM sometimes.
			// @see https://docs.oracle.com/javase/8/docs/technotes/guides/troubleshoot/envvars002.html
			content = content.split('\n')
				.filter(s -> 0 != s.indexOf('Picked up JAVA_TOOL_OPTIONS:'))
				.join('\n');

			if (content != expected) {
				final a = new diff.FileData(Bytes.ofString(expected), "expected", Date.now());
				final b = new diff.FileData(Bytes.ofString(content), "actual", Date.now());
				var ctx:diff.Context = {
					file1: a,
					file2: b,
					context: 10
				}
				final script = diff.Analyze.diff2Files(ctx);
				var diff = diff.Printer.printUnidiff(ctx, script);
				diff = diff.split("\n").slice(3).join("\n");
				println(diff);
				return false;
			}
		}

		return true;
	}

	static macro function getStd() {
		var std = Compiler.getConfiguration().stdPath;
		return macro $v{std.shift()};
	}
}
