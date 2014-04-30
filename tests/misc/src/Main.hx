import sys.FileSystem;
import haxe.io.Path;
import haxe.macro.Expr;

using StringTools;

typedef Result = {
	count: Int,
	failures: Int
}

class Main {
	static public function main() {
		var result:Result = compileProjects();
		Sys.println('Done running ${result.count} tests with ${result.failures} failures');
		Sys.exit(result.failures);
	}

	macro static public function compileProjects():ExprOf<Result> {
		var count = 0;
		var failures = 0;
		function browse(dirPath) {
			var dir = FileSystem.readDirectory(dirPath);
			for (file in dir) {
				var path = Path.join([dirPath, file]);
				if (FileSystem.isDirectory(path)) {
					browse(path);
				} else if (file.endsWith(".hxml")) {
					var old = Sys.getCwd();
					Sys.setCwd(dirPath);
					Sys.println('Running haxe $path');
					var expectFailure = file.endsWith("-fail.hxml");
					var success = runCommand("haxe", [file], expectFailure);
					++count;
					if (!success) {
						failures++;
					}
					Sys.setCwd(old);
				}
			}
		}
		browse("projects");
		return macro $v{
			{
				count: $v{count},
				failures: $v{failures}
			}
		};
	}

	static function runCommand(command:String, args:Array<String>, expectFailure:Bool) {
		var proc = new sys.io.Process(command, args);
		var exit = proc.exitCode();
		var success = exit == 0;
		return switch [success, expectFailure] {
			case [true, false]:
				true;
			case [true, true]:
				Sys.println("Expected failure, but no failure occured");
				false;
			case [false, true]:
				true;
			case [false, false]:
				var stderr = proc.stderr.readAll().toString();
				Sys.print(stderr);
				false;
		}
	}
}