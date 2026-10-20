import haxe.io.Path;
import sys.FileSystem;
import sys.io.File;
import sys.io.Process;

using StringTools;

/**
	Incremental recompilation test runner.

	Each case (`cases/**.hxtest`) describes an initial set of files followed by
	edit steps. For every step, the runner compiles the project twice:

	- **warm**: through a compilation server that has seen all previous steps;
	- **cold**: with a fresh compiler process, in a separate directory holding
	  exactly the same files.

	The main oracle is equivalence: exit code, compiler messages, program
	output and every generated file must be identical between warm and cold.
	Step expectations (`@expect-*`) additionally check that the scenario
	exercises what it is meant to.

	Usage (from `tests/incremental`):
	  haxe run.hxml [-- options]
	Options:
	  --filter <substring>  only run cases whose path contains the substring
	  --haxe <path>         compiler to test (default: `haxe` from PATH)
	  --targets <list>      restrict targets (comma separated: interp,js)
	  --strict-order        compare compiler messages without sorting them
	  --invalidate          notify the server of changed files (`server/invalidate`)
	                        instead of relying on modification times
	  --keep                keep the work directories
	  --verbose             print every compilation
	  --show-known          print the errors of known failures
	  --real-mtime          keep real modification times for every case (see @real-mtime)
	  --args "<args>"        extra compiler arguments for every compilation
**/
class RunIncremental {
	public static var haxeCmd = "haxe";
	/** Whether `compile` runs `out/main.js` with node. **/
	public static var runProgram = true;
	/** Compares generated text files as sorted lines (differences of order only are ignored). **/
	public static var ignoreOrder = false;
	static var filter:Null<String> = null;
	static var targetFilter:Null<Array<String>> = null;
	static var strictOrder = false;
	static var invalidate = false;
	static var keep = false;
	static var verbose = false;
	static var showKnown = false;
	static var realMtime = false;
	static var extraArgs:Array<String> = [];
	static var root:String;
	/** Fake clock for modification times, see `touchStep`. **/
	static var clock:Float = Math.ffloor(Date.now().getTime() / 1000) + 10;
	static var workRoot:String;

	static function main() {
		var args = Sys.args();
		var i = 0;
		while (i < args.length) {
			switch (args[i]) {
				case "--filter":
					filter = args[++i];
				case "--haxe":
					haxeCmd = args[++i];
				case "--targets":
					targetFilter = args[++i].split(",");
				case "--strict-order":
					strictOrder = true;
				case "--invalidate":
					invalidate = true;
				case "--keep":
					keep = true;
				case "--verbose":
					verbose = true;
				case "--show-known":
					showKnown = true;
				case "--real-mtime":
					realMtime = true;
				case "--args":
					extraArgs = extraArgs.concat(args[++i].split(" ").filter(s -> s != ""));
				case other:
					Sys.println('Unknown option $other');
					Sys.exit(2);
			}
			i++;
		}
		root = Path.normalize(Sys.getCwd());
		workRoot = Path.join([root, "work"]);
		removeDir(workRoot);
		FileSystem.createDirectory(workRoot);

		var files = [];
		collectCases(Path.join([root, "cases"]), files);
		files.sort(Reflect.compare);

		var results = [];
		var port = 7400 + Std.random(500);
		for (file in files) {
			var rel = file.substr(root.length + 1);
			if (filter != null && rel.indexOf(filter) < 0)
				continue;
			var tc = try {
				CaseParser.parse(rel, File.getContent(file));
			} catch (e:CaseParser.ParseError) {
				results.push({name: rel, target: "-", outcome: Failed(['parse error: ${e.message}'])});
				continue;
			}
			for (target in tc.targets) {
				if (targetFilter != null && !targetFilter.contains(target))
					continue;
				var outcome = runCase(tc, target, port++);
				results.push({name: rel, target: target, outcome: outcome});
				printOutcome(rel, target, outcome);
			}
		}

		var passed = 0, failed = 0, xfail = 0, xpass = 0;
		for (r in results) {
			switch (r.outcome) {
				case Passed: passed++;
				case Failed(_): failed++;
				case KnownFailure(_, _): xfail++;
				case UnexpectedPass: xpass++;
			}
		}
		Sys.println("");
		Sys.println('$passed passed, $failed failed, $xfail known failures, $xpass unexpected passes');
		if (!keep)
			removeDir(workRoot);
		Sys.exit(failed + xpass > 0 ? 1 : 0);
	}

	static function printOutcome(name:String, target:String, outcome:Outcome) {
		switch (outcome) {
			case Passed:
				Sys.println('PASS   $name [$target]');
			case KnownFailure(reason, errors):
				Sys.println('XFAIL  $name [$target] ($reason)');
				if (showKnown)
					for (e in errors)
						Sys.println("       " + e.split("\n").join("\n       "));
			case UnexpectedPass:
				Sys.println('XPASS  $name [$target] (marked @known-failure but passed: remove the marker)');
			case Failed(errors):
				Sys.println('FAIL   $name [$target]');
				for (e in errors)
					Sys.println("       " + e.split("\n").join("\n       "));
		}
	}

	static function collectCases(dir:String, acc:Array<String>) {
		if (!FileSystem.exists(dir))
			return;
		for (entry in FileSystem.readDirectory(dir)) {
			var path = Path.join([dir, entry]);
			if (FileSystem.isDirectory(path))
				collectCases(path, acc);
			else if (entry.endsWith(".hxtest"))
				acc.push(path);
		}
	}

	static function runCase(tc:CaseParser.TestCase, target:String, port:Int):Outcome {
		var base = Path.join([workRoot, tc.name.replace("/", "_").replace(".hxtest", ""), target]);
		var warmDir = Path.join([base, "warm"]);
		var coldDir = Path.join([base, "cold"]);
		FileSystem.createDirectory(warmDir);
		FileSystem.createDirectory(coldDir);

		var server = new Process(haxeCmd, ["--wait", Std.string(port)]);
		// Give the server time to listen.
		var errors = [];
		var connected = false;
		for (_ in 0...50) {
			var probe = new Process(haxeCmd, ["--connect", Std.string(port), "--version"]);
			var code = probe.exitCode();
			probe.close();
			if (code == 0) {
				connected = true;
				break;
			}
			Sys.sleep(0.1);
		}
		if (!connected) {
			server.kill();
			server.close();
			return Failed(['could not start compilation server on port $port']);
		}

		try {
			for (stepIndex in 0...tc.steps.length) {
				var step = tc.steps[stepIndex];
				var label = 'step $stepIndex "${step.name}"';
				var changed = [];
				clock += 2;
				for (dir in [warmDir, coldDir]) {
					changed = applyOps(dir, step, tc, target, dir == warmDir ? port : null, warmDir);
					if (!tc.realMtime && !realMtime)
						touchStep(dir, changed);
				}
				if (invalidate) {
					for (file in changed)
						notifyInvalidate(port, warmDir, file);
				}
				var warm = compile(warmDir, port);
				var cold = compile(coldDir, null);
				if (verbose) {
					Sys.println('--- ${tc.name} [$target] $label');
					Sys.println('warm exit ${warm.code}\n${warm.out}');
					Sys.println('cold exit ${cold.code}\n${cold.out}');
				}
				for (e in compare(warm, cold, warmDir, coldDir))
					errors.push('$label: $e');
				for (e in checkExpectations(step.expectations, warm, warmDir, target))
					errors.push('$label: $e');
				if (step.expectations.contains(NoFsCalls)) {
					var calls = noChangeCompilationCalls(warmDir, port);
					if (calls != 0)
						errors.push('$label: a compilation without changes made $calls file system calls, expected 0');
				}
				if (errors.length > 0)
					break;
			}
		} catch (e:haxe.Exception) {
			errors.push('runner error: ${e.message}');
		}
		server.kill();
		server.close();

		return switch [errors.length, tc.knownFailureFor(target)] {
			case [0, null]: Passed;
			case [0, _]: UnexpectedPass;
			case [_, null]: Failed(errors);
			case [_, reason]: KnownFailure(reason, errors);
		}
	}

	/**
		Applies the file operations of a step to [dir]. Overlays are sent to the
		server for the warm directory ([port] given), and written to disk for the
		cold one; removing an overlay restores the cold file from the warm
		directory's disk content.
	**/
	static function applyOps(dir:String, step:CaseParser.Step, tc:CaseParser.TestCase, target:String, port:Null<Int>, warmDir:String):Array<String> {
		var changed = [];
		for (op in step.ops) {
			switch (op) {
				case Write(path, content):
					var full = Path.join([dir, path]);
					FileSystem.createDirectory(Path.directory(full));
					File.saveContent(full, content);
					changed.push(path);
				case Delete(path):
					var full = Path.join([dir, path]);
					if (FileSystem.exists(full))
						FileSystem.deleteFile(full);
					changed.push(path);
				case Overlay(path, content):
					var full = Path.join([dir, path]);
					if (port != null) {
						sendOverlay(port, full, content);
					} else {
						FileSystem.createDirectory(Path.directory(full));
						File.saveContent(full, content);
						changed.push(path);
					}
				case RemoveOverlay(path):
					var full = Path.join([dir, path]);
					if (port != null) {
						sendOverlay(port, full, null);
					} else {
						var disk = Path.join([warmDir, path]);
						if (FileSystem.exists(disk))
							File.saveContent(full, File.getContent(disk));
						else if (FileSystem.exists(full))
							FileSystem.deleteFile(full);
						changed.push(path);
					}
			}
		}
		// The build file is regenerated for every step so that `@args` changes apply.
		File.saveContent(Path.join([dir, "build.hxml"]), tc.hxmlFor(step, target) + extraArgs.join("\n") + "\n");
		return changed;
	}

	/**
		The compilation server compares modification times with a precision of
		one second, so edits made within the same second as the previous
		compilation are not detected (see the `real-mtime` cases). Unless a case
		asks for real modification times, every step gets its own modification
		time, two seconds after the previous step, on the changed files and their
		parent directories (directory times are used to detect new files).
	**/
	static function touchStep(dir:String, changed:Array<String>) {
		var targets = [Path.join([dir, "build.hxml"])];
		for (file in changed) {
			var full = Path.join([dir, file]);
			if (FileSystem.exists(full))
				targets.push(full);
			var parent = Path.directory(full);
			while (parent.length >= dir.length) {
				if (!targets.contains(parent))
					targets.push(parent);
				if (parent == dir)
					break;
				parent = Path.directory(parent);
			}
		}
		var p = new Process("touch", ["-m", "-d", "@" + Std.string(Std.int(clock))].concat(targets));
		p.exitCode();
		p.close();
	}

	/**
		Compiles the warm project again without any change, until the watches of
		the virtual file system are active (a few compilations), and returns the
		number of file system calls of the last compilation (`server/vfs`).
	**/
	static function noChangeCompilationCalls(dir:String, port:Int):Int {
		var calls = -1;
		for (_ in 0...5) {
			compile(dir, port, false);
			var request = haxe.Json.stringify({jsonrpc: "2.0", id: 1, method: "server/vfs", params: {}});
			var p = new Process(haxeCmd, ["--connect", Std.string(port), "--display", request]);
			var out = p.stderr.readAll().toString();
			p.exitCode();
			p.close();
			calls = try haxe.Json.parse(out).result.result.fileSystemCalls catch (_) -1;
			if (calls == 0)
				break;
		}
		return calls;
	}

	static function sendOverlay(port:Int, file:String, content:Null<String>) {
		var params:Dynamic = {file: file};
		if (content != null)
			params.contents = content;
		var request = haxe.Json.stringify({jsonrpc: "2.0", id: 1, method: "server/overlay", params: params});
		var p = new Process(haxeCmd, ["--connect", Std.string(port), "--display", request]);
		p.exitCode();
		p.close();
	}

	static function notifyInvalidate(port:Int, dir:String, file:String) {
		var request = haxe.Json.stringify({
			jsonrpc: "2.0",
			id: 1,
			method: "server/invalidate",
			params: {file: Path.join([dir, file])}
		});
		var p = new Process(haxeCmd, ["--connect", Std.string(port), "--cwd", dir, "--display", request]);
		p.exitCode();
		p.close();
	}

	public static function compile(dir:String, port:Null<Int>, clean = true):CompileResult {
		var outDir = Path.join([dir, "out"]);
		if (clean) {
			removeDir(outDir);
			FileSystem.createDirectory(outDir);
		}
		var args = port == null ? [] : ["--connect", Std.string(port)];
		args = args.concat(["--cwd", dir, "build.hxml"]);
		var p = new Process(haxeCmd, args);
		var stdout = p.stdout.readAll().toString();
		var stderr = p.stderr.readAll().toString();
		var code = p.exitCode();
		p.close();
		var out = normalize(stdout + stderr, dir);
		var run = "";
		var runner = Path.join([dir, "out", "main.js"]);
		if (runProgram && code == 0 && FileSystem.exists(runner)) {
			var node = new Process("node", [runner]);
			run = node.stdout.readAll().toString() + node.stderr.readAll().toString();
			node.exitCode();
			node.close();
			run = normalize(run, dir);
		}
		return {
			code: code,
			out: out,
			run: run,
			files: listFiles(outDir, "")
		};
	}

	static function normalize(s:String, dir:String) {
		s = s.replace("\r\n", "\n");
		s = s.replace(dir + "/", "").replace(dir, ".");
		return s.trim();
	}

	static function listFiles(dir:String, prefix:String):Array<String> {
		var acc = [];
		if (!FileSystem.exists(dir))
			return acc;
		var entries = FileSystem.readDirectory(dir);
		entries.sort(Reflect.compare);
		for (entry in entries) {
			var full = Path.join([dir, entry]);
			var rel = prefix == "" ? entry : prefix + "/" + entry;
			if (FileSystem.isDirectory(full))
				acc = acc.concat(listFiles(full, rel));
			else
				acc.push(rel);
		}
		return acc;
	}

	static function lines(s:String) {
		var l = s.split("\n").map(StringTools.trim).filter(x -> x != "");
		if (!strictOrder)
			l.sort(Reflect.compare);
		return l;
	}

	public static function compare(warm:CompileResult, cold:CompileResult, warmDir:String, coldDir:String):Array<String> {
		var errors = [];
		if (warm.code != cold.code)
			errors.push('exit code differs: warm ${warm.code}, cold ${cold.code}');
		var wl = lines(warm.out), cl = lines(cold.out);
		if (wl.join("\n") != cl.join("\n"))
			errors.push('compiler output differs\n' + diff(cl, wl));
		if (warm.run != cold.run)
			errors.push('program output differs\n' + diff(cold.run.split("\n"), warm.run.split("\n")));
		if (warm.files.join(",") != cold.files.join(","))
			errors.push('generated files differ: warm [${warm.files.join(", ")}], cold [${cold.files.join(", ")}]');
		else {
			for (f in warm.files) {
				var a = File.getBytes(Path.join([warmDir, "out", f]));
				var b = File.getBytes(Path.join([coldDir, "out", f]));
				if (isText(f)) {
					// Generated text may contain the absolute path of the project
					// (e.g. source maps), which differs between warm and cold.
					var sa = a.toString().replace(warmDir, "<dir>");
					var sb = b.toString().replace(coldDir, "<dir>");
					if (ignoreOrder) {
						function sorted(s:String) {
							var l = s.split("\n");
							l.sort(Reflect.compare);
							return l.join("\n");
						}
						sa = sorted(sa);
						sb = sorted(sb);
					}
					a = haxe.io.Bytes.ofString(sa);
					b = haxe.io.Bytes.ofString(sb);
				}
				if (a.compare(b) != 0) {
					var detail = "";
					if (isText(f))
						detail = "\n" + diff(b.toString().split("\n"), a.toString().split("\n"));
					errors.push('generated file out/$f differs$detail');
				}
			}
		}
		return errors;
	}

	static function isText(f:String) {
		return f.endsWith(".js") || f.endsWith(".map") || f.endsWith(".txt") || f.endsWith(".json");
	}

	static function diff(cold:Array<String>, warm:Array<String>):String {
		var out = [];
		var max = cold.length > warm.length ? cold.length : warm.length;
		var shown = 0;
		for (i in 0...max) {
			var c = i < cold.length ? cold[i] : "<none>";
			var w = i < warm.length ? warm[i] : "<none>";
			if (c != w) {
				out.push('  line ${i + 1}: cold: $c');
				out.push('  line ${i + 1}: warm: $w');
				if (++shown >= 5) {
					out.push("  ...");
					break;
				}
			}
		}
		return out.join("\n");
	}

	static function checkExpectations(expectations:Array<CaseParser.Expectation>, r:CompileResult, dir:String, target:String):Array<String> {
		var errors = [];
		var output = target == "interp" ? r.out : r.run;
		for (e in expectations) {
			switch (e) {
				case Success:
					if (r.code != 0)
						errors.push('expected success, got exit code ${r.code}:\n${r.out}');
				case Error(text):
					if (r.code == 0)
						errors.push('expected an error containing "$text", compilation succeeded');
					else if (r.out.indexOf(text) < 0)
						errors.push('expected an error containing "$text", got:\n${r.out}');
				case Warning(text):
					if (r.out.indexOf(text) < 0)
						errors.push('expected a warning containing "$text", got:\n${r.out}');
				case NoWarning:
					if (r.out.indexOf("Warning") >= 0 || r.out.indexOf("warning") >= 0)
						errors.push('expected no warning, got:\n${r.out}');
				case NoFsCalls:
					// Checked separately, see noChangeCompilationCalls.
				case Output(text):
					if (target == "interp" || target == "js") {
						if (output.indexOf(text) < 0)
							errors.push('expected program output containing "$text", got:\n$output');
					}
				case CompilerOutput(text):
					if (r.out.indexOf(text) < 0)
						errors.push('expected compiler output containing "$text", got:\n${r.out}');
				case NoOutput(text):
					if (output.indexOf(text) >= 0)
						errors.push('expected program output without "$text", got:\n$output');
				case FileContains(path, text):
					var full = Path.join([dir, path]);
					if (!FileSystem.exists(full))
						errors.push('expected file $path to exist');
					else if (File.getContent(full).indexOf(text) < 0)
						errors.push('expected file $path to contain "$text"');
				case FileNotContains(path, text):
					var full = Path.join([dir, path]);
					if (FileSystem.exists(full) && File.getContent(full).indexOf(text) >= 0)
						errors.push('expected file $path not to contain "$text"');
			}
		}
		return errors;
	}

	public static function removeDir(dir:String) {
		if (!FileSystem.exists(dir))
			return;
		for (entry in FileSystem.readDirectory(dir)) {
			var path = Path.join([dir, entry]);
			if (FileSystem.isDirectory(path))
				removeDir(path);
			else
				FileSystem.deleteFile(path);
		}
		FileSystem.deleteDirectory(dir);
	}
}

typedef CompileResult = {
	code:Int,
	out:String,
	run:String,
	files:Array<String>
}

enum Outcome {
	Passed;
	Failed(errors:Array<String>);
	KnownFailure(reason:String, errors:Array<String>);
	UnexpectedPass;
}
