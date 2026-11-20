import Platform;
import haxe.io.Path;
import hxcoro.Coro;
import hxcoro.Coro.*;
import hxcoro.concurrent.CoroSemaphore;
import hxcoro.task.ICoroNode;
import sys.FileSystem;
import sys.io.File;

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

	The runner runs on node, or on the JVM (`build-jvm.hxml`), see `Platform`;
	the cases run at the same time (`--jobs`), each in its own directories with
	its own server.

	Usage (from `tests/incremental`):
	  haxe run.hxml                         build and run all cases
	  haxe build.hxml
	  node bin/incremental.js [options]
	Options:
	  --filter <substring>  only run cases whose path contains the substring
	  --haxe <path>         compiler to test (default: `haxe` from PATH)
	  --targets <list>      restrict targets (comma separated: interp,js,hl)
	  --add-target <target> also run every case on this target (e.g. hl)
	  --strict-order        compare compiler messages without sorting them
	  --invalidate          notify the server of changed files (`server/invalidate`)
	                        instead of relying on modification times
	  --keep                keep the work directories
	  --verbose             print every compilation
	  --show-known          print the errors of known failures
	  --real-mtime          keep real modification times for every case (see @real-mtime)
	  --check-deps          check that the dependencies recorded by the compiler are
	                        complete (needs `-D dump-fingerprints`)
	  --args "<args>"        extra compiler arguments for every compilation
	  --jobs <n>            number of cases run at the same time (default: the
	                        number of processors; 1 with --verbose)
	  --timeout <seconds>   time after which a case fails (default: 300)
	  --vfs <mode>          file system mode of the server (`HAXE_VFS`), e.g. `watch`
**/
class RunIncremental {
	public static var haxeCmd = "haxe";
	/** Whether `compile` runs `out/main.js` with node. **/
	public static var runProgram = true;
	/** Compares generated text files as sorted lines (differences of order only are ignored). **/
	public static var ignoreOrder = false;
	static var filter:Null<String> = null;
	static var targetFilter:Null<Array<String>> = null;
	static var addedTargets:Array<String> = [];
	static var strictOrder = false;
	static var invalidate = false;
	static var keep = false;
	static var verbose = false;
	static var showKnown = false;
	static var realMtime = false;
	static var checkDeps = false;
	static var extraArgs:Array<String> = [];
	static var jobs:Null<Int> = null;
	static var timeout = 300;
	/** The file system mode of the server: `legacy`, or `watch` (see `--vfs`). **/
	static var vfsMode = "legacy";
	/** Compiler features some expectations need, detected by `detectFeatures`. **/
	static var features:Map<String, Bool> = [];
	static var root:String;
	static var workRoot:String;

	static function main() {
		Platform.run(runAll);
	}

	@:coroutine static function runAll(node:ICoroNode):Int {
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
				case "--add-target":
					addedTargets.push(args[++i]);
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
				case "--check-deps":
					checkDeps = true;
				case "--vfs":
					// inherited by the servers the runner starts
					Sys.putEnv("HAXE_VFS", args[++i]);
				case "--args":
					extraArgs = extraArgs.concat(args[++i].split(" ").filter(s -> s != ""));
				case "--jobs":
					jobs = Std.parseInt(args[++i]);
				case "--timeout":
					timeout = Std.parseInt(args[++i]);
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

		detectFeatures();
		if (features["server/vfs"])
			vfsMode = "watch";
		if (Sys.getEnv("HAXE_VFS") == "watch" && vfsMode != "watch") {
			Sys.println("HAXE_VFS=watch is not supported by this compiler");
			Sys.exit(2);
		}
		Sys.println('File system mode of the server: $vfsMode');
		if (checkDeps && !features["dump-fingerprints"]) {
			Sys.println("--check-deps needs a compiler supporting -D dump-fingerprints");
			Sys.exit(2);
		}

		var files = [];
		collectCases(Path.join([root, "cases"]), files);
		files.sort(Reflect.compare);

		var results = [];
		var runs = [];
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
			var targets = tc.targets.concat(addedTargets.filter(t -> !tc.targets.contains(t)));
			for (target in targets) {
				if (targetFilter != null && !targetFilter.contains(target))
					continue;
				runs.push({name: rel, target: target, tc: tc, skipped: []});
			}
		}
		// The cases are independent (own directories, server and port), so
		// several run at the same time; the outcomes are printed in order.
		if (jobs == null)
			jobs = verbose ? 1 : Platform.cpuCount();
		var slots = new CoroSemaphore(jobs < 1 ? 1 : jobs);
		var port = 7400 + Std.random(500);
		var tasks = [
			for (index => run in runs)
				node.async(_ -> {
					slots.acquire();
					var outcome = try runCase(run.tc, run.target, port + index, run.skipped) catch (e:haxe.Exception) Failed(['runner error: ${e.message}']);
					slots.release();
					outcome;
				})
		];
		for (index => run in runs) {
			var outcome = tasks[index].await();
			results.push({name: run.name, target: run.target, outcome: outcome});
			printOutcome(run.name, run.target, outcome);
		}

		// the features some expectations needed and the compiler does not have
		var unsupported = [];
		var skippedExpectations = 0;
		for (run in runs) {
			skippedExpectations += run.skipped.length;
			for (feature in run.skipped)
				if (!unsupported.contains(feature))
					unsupported.push(feature);
		}
		var passed = 0, failed = 0, xfail = 0, xpass = 0, skipped = 0;
		for (r in results) {
			switch (r.outcome) {
				case Passed: passed++;
				case Skipped(_): skipped++;
				case Failed(_): failed++;
				case KnownFailure(_, _): xfail++;
				case UnexpectedPass: xpass++;
			}
		}
		Sys.println("");
		Sys.println('$passed passed, $failed failed, $xfail known failures, $xpass unexpected passes, $skipped skipped');
		if (skippedExpectations > 0)
			Sys.println('$skippedExpectations expectations not checked (not supported by this compiler: ${unsupported.join(", ")})');
		if (!keep)
			removeDir(workRoot);
		return failed + xpass > 0 ? 1 : 0;
	}

	static function printOutcome(name:String, target:String, outcome:Outcome) {
		switch (outcome) {
			case Passed:
				Sys.println('PASS   $name [$target]');
			case Skipped(reason):
				Sys.println('SKIP   $name [$target] ($reason)');
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

	/** What `haxe --version` prints, set by `detectFeatures`. **/
	static var version:String;

	/**
		Detects the compiler features that some expectations need: the
		`dump-fingerprints` define and the server methods `server/retyped`,
		`server/vfs` and `server/overlay`. Expectations needing a missing
		feature are not checked, and cases with overlays are skipped.
	**/
	@:coroutine static function detectFeatures() {
		var p = Proc.run(haxeCmd, ["--version"]);
		version = (p.stdout() + p.stderr()).trim();
		features["dump-fingerprints"] = Proc.run(haxeCmd, ["--help-defines"]).stdout().indexOf("dump-fingerprints") >= 0;
		hasHl();
		var methods = ["server/retyped", "server/vfs", "server/overlay"];
		var port = 7300 + Std.random(90);
		while (Platform.portInUse(port))
			port++;
		var server = new Proc(haxeCmd, ["--wait", Std.string(port)]);
		waitForServer(port);
		for (method in methods) {
			var request = haxe.Json.stringify({jsonrpc: "2.0", id: 1, method: method, params: {}});
			var p = Proc.run(haxeCmd, ["--connect", Std.string(port), "--display", request]);
			var err = p.stderr();
			var out = err + p.stdout();
			// -32601: method not found
			features[method] = out.indexOf("-32601") < 0 && out.indexOf("jsonrpc") >= 0;
			if (method == "server/vfs") {
				// the mode of the server, and whether it sees changes from file system events
				var result:Dynamic = try haxe.Json.parse(err).result.result catch (_) null;
				features[method] = result != null && result.mode == "watch";
				features["file system events"] = features[method] && result.watching == true;
			}
		}
		server.kill();
		server.exitCode();
	}

	/** Whether [feature] is supported; adds it to [skipped] otherwise (an expectation not checked). **/
	static function supports(feature:String, skipped:Array<String>):Bool {
		if (features[feature])
			return true;
		skipped.push(feature);
		return false;
	}

	/**
		Whether the server on [port] answers a client with the version of the
		compiler. A server that hangs (or another program on the port) may accept
		the client and never answer, so the client is given a few seconds.
	**/
	@:coroutine static function serverAnswers(port:Int):Bool {
		return try Coro.timeout(10000, _ -> {
			var probe = Proc.run(haxeCmd, ["--connect", Std.string(port), "--version"]);
			probe.exitCode() == 0 && (probe.stdout() + probe.stderr()).trim() == version;
		}) catch (_:hxcoro.TimeoutException) false;
	}

	/** Waits for the server on [port] to answer with the version of the compiler. **/
	@:coroutine static function waitForServer(port:Int):Bool {
		for (_ in 0...50) {
			if (serverAnswers(port))
				return true;
			delay(100);
		}
		return false;
	}

	/**
		Runs a case on [target]. The features that some of its expectations
		needed and the compiler does not have are added to [skipped], once per
		expectation not checked.
	**/
	@:coroutine static function runCase(tc:CaseParser.TestCase, target:String, port:Int, skipped:Array<String>):Outcome {
		var usesOverlays = Lambda.exists(tc.steps, step -> Lambda.exists(step.ops, op -> op.match(Overlay(_, _) | RemoveOverlay(_))));
		if (usesOverlays && !supports("server/overlay", skipped))
			return Skipped("needs server/overlay");
		// symbolic links are made with `ln`
		if (Sys.systemName() == "Windows" && Lambda.exists(tc.steps, step -> Lambda.exists(step.ops, op -> op.match(Symlink(_, _)))))
			return Skipped("needs symbolic links");
		var base = Path.join([workRoot, tc.name.replace("/", "_").replace(".hxtest", ""), target]);
		var warmDir = Path.join([base, "warm"]);
		var coldDir = Path.join([base, "cold"]);
		FileSystem.createDirectory(warmDir);
		FileSystem.createDirectory(coldDir);

		// what the server prints is shown when the case fails (a server that dies
		// during a compilation gives the client an empty output and exit code 0)
		var server:Null<Proc> = null;
		var errors = [];
		var connected = false;
		// A port may be taken (by another program, which can answer the probe
		// with an empty output and exit code 0): the probe must get the version of
		// the compiler, otherwise the server is stopped and another port tried.
		for (attempt in 0...5) {
			if (attempt > 0)
				port += 1000;
			// a program already listening may accept the probe and never answer
			// (Windows Delivery Optimization on 7680): skip a port in use
			if (Platform.portInUse(port))
				continue;
			server = new Proc(haxeCmd, ["--wait", Std.string(port)]);
			connected = waitForServer(port);
			if (connected)
				break;
			server.kill();
			server.exitCode();
		}
		if (!connected)
			return Failed(['could not start compilation server on port $port']);

		// with fingerprint expectations, the cold compilations dump their fingerprints
		var fingerprints = checkDeps || Lambda.exists(tc.steps, step -> Lambda.exists(step.expectations, e -> e.match(FingerprintSame(_, _) | FingerprintChanged(_, _))));
		fingerprints = fingerprints && features["dump-fingerprints"];
		var previousDeps:Null<Map<String, Array<String>>> = null;
		var dumpFile = Path.join([base, "fingerprints.txt"]);
		var previousFingerprints:Null<Map<String, String>> = null;
		// Fake clock for modification times, see `touchStep`.
		var clock = Math.ffloor(Date.now().getTime() / 1000) + 10;
		// Tenths of a second after `clock` (see `@same-second`).
		var clockTenths = 0;
		var label = "";
		try {
			// a case that hangs (e.g. a server that never answers) fails instead of
			// blocking the run; the processes it waits for are killed
			Coro.timeout(timeout * 1000, _ -> for (stepIndex in 0...tc.steps.length) {
				var step = tc.steps[stepIndex];
				label = 'step $stepIndex "${step.name}"';
				var changed = [];
				// a new second, or a fraction of a second later in the same one
				if (step.sameSecond) {
					if (clockTenths >= 9)
						throw new haxe.Exception("too many @same-second steps in a row");
					clockTenths += 3;
				} else {
					clock += 2;
					clockTenths = 0;
				}
				for (dir in [warmDir, coldDir]) {
					var structural = [];
					changed = applyOps(dir, step, tc, target, dir == warmDir ? port : null, warmDir, structural);
					if (!tc.realMtime && !realMtime)
						touchStep(dir, changed, structural, clock + clockTenths / 10);
				}
				if (invalidate) {
					for (file in changed)
						notifyInvalidate(port, warmDir, file);
				}
				var warm = compile(warmDir, port);
				for (e in step.expectations)
					switch (e) {
						case Retyped(_) if (!supports("server/retyped", skipped)):
						case Retyped(expected):
							var retyped = retypedModules(warmDir, port);
							if (retyped.join(" ") != expected.join(" "))
								errors.push('$label: expected the modules [${expected.join(" ")}] to be typed again, got [${retyped.join(" ")}]');
						case _:
					}
				if (FileSystem.exists(dumpFile))
					FileSystem.deleteFile(dumpFile);
				var cold = compile(coldDir, null, true, fingerprints ? ["-D", "dump-fingerprints=" + dumpFile] : []);
				var currentFingerprints = fingerprints && FileSystem.exists(dumpFile) ? readFingerprints(dumpFile) : null;
				for (e in step.expectations) {
					switch (e) {
						case FingerprintSame(_, _) | FingerprintChanged(_, _) if (!supports("dump-fingerprints", skipped)):
						case FingerprintSame(node, key) | FingerprintChanged(node, key):
							var same = e.match(FingerprintSame(_, _));
							var before = previousFingerprints == null ? null : findFingerprint(previousFingerprints, node, key);
							var after = currentFingerprints == null ? null : findFingerprint(currentFingerprints, node, key);
							if (before == null || after == null)
								errors.push('$label: no $node fingerprint of $key' + (before == null ? " in the previous step" : ""));
							else if (same && before != after)
								errors.push('$label: expected the $node fingerprint of $key to stay the same');
							else if (!same && before == after)
								errors.push('$label: expected the $node fingerprint of $key to change');
						case _:
					}
				}
				if (checkDeps && previousFingerprints != null && previousDeps != null && currentFingerprints != null)
					for (e in missingDependencies(previousFingerprints, previousDeps, currentFingerprints))
						errors.push('$label: $e');
				previousFingerprints = currentFingerprints;
				previousDeps = fingerprints && FileSystem.exists(dumpFile) ? readDependencies(dumpFile) : null;
				if (verbose) {
					Sys.println('--- ${tc.name} [$target] $label');
					Sys.println('warm exit ${warm.code}\n${warm.out}');
					Sys.println('cold exit ${cold.code}\n${cold.out}');
				}
				for (e in compare(warm, cold, warmDir, coldDir))
					errors.push('$label: $e');
				for (e in checkExpectations(step.expectations, warm, warmDir, target))
					errors.push('$label: $e');
				if (step.expectations.contains(NoFsCalls) && supports("server/vfs", skipped) && supports("file system events", skipped)) {
					var calls = noChangeCompilationCalls(warmDir, port);
					if (calls != 0)
						errors.push('$label: a compilation without changes made $calls file system calls, expected 0');
				}
				if (errors.length > 0)
					break;
			});
		} catch (e:hxcoro.TimeoutException) {
			errors.push('$label: timed out after $timeout seconds');
		} catch (e:haxe.Exception) {
			errors.push('runner error: ${e.message}');
		}
		if (errors.length > 0) {
			if (!serverAnswers(port))
				errors.push('the compilation server no longer answers');
		}
		server.kill();
		server.exitCode();
		if (errors.length > 0) {
			var output = (server.stdout() + server.stderr()).trim();
			if (output != "") {
				var lines = output.split("\n");
				if (lines.length > 40)
					lines = ["..."].concat(lines.slice(lines.length - 40));
				errors.push('server output:\n' + lines.join("\n"));
			}
		}

		return switch [errors.length, tc.knownFailureFor(target, vfsMode)] {
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
		directory's disk content. Returns the changed paths; the ones that were
		created or deleted are added to [structural].
	**/
	@:coroutine static function applyOps(dir:String, step:CaseParser.Step, tc:CaseParser.TestCase, target:String, port:Null<Int>, warmDir:String, structural:Array<String>):Array<String> {
		var changed = [];
		function write(path:String, content:String) {
			var full = Path.join([dir, path]);
			if (!FileSystem.exists(full))
				structural.push(path);
			FileSystem.createDirectory(Path.directory(full));
			File.saveContent(full, content);
			changed.push(path);
		}
		function delete(path:String) {
			var full = Path.join([dir, path]);
			if (FileSystem.exists(full)) {
				FileSystem.deleteFile(full);
				structural.push(path);
			}
			changed.push(path);
		}
		for (op in step.ops) {
			switch (op) {
				case Write(path, content):
					write(path, content);
				case Delete(path):
					delete(path);
				case DeleteDir(path):
					removeDir(Path.join([dir, path]));
					changed.push(path);
					structural.push(path);
				case Symlink(path, target):
					var full = Path.join([dir, path]);
					FileSystem.createDirectory(Path.directory(full));
					if (Proc.run("ln", ["-sfn", Path.join([dir, target]), full]).exitCode() != 0)
						throw new haxe.Exception('could not create the symbolic link $path');
					changed.push(path);
					structural.push(path);
				case Overlay(path, content):
					var full = Path.join([dir, path]);
					if (port != null)
						sendOverlay(port, full, content);
					else
						write(path, content);
				case RemoveOverlay(path):
					var full = Path.join([dir, path]);
					if (port != null) {
						sendOverlay(port, full, null);
					} else {
						var disk = Path.join([warmDir, path]);
						if (FileSystem.exists(disk))
							write(path, File.getContent(disk));
						else
							delete(path);
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
		time, two seconds after the previous step (a fraction of a second after it
		for a `@same-second` step), on the changed files and, for the ones that
		were created or deleted ([structural]), on their parent directories
		(directory times are used to detect new files). The directory of a file
		that is only modified keeps its time, as on a real file system.
	**/
	static function touchStep(dir:String, changed:Array<String>, structural:Array<String>, time:Float) {
		var targets = [Path.join([dir, "build.hxml"])];
		for (file in changed) {
			var full = Path.join([dir, file]);
			if (FileSystem.exists(full))
				targets.push(full);
			if (!structural.contains(file))
				continue;
			var parent = Path.directory(full);
			while (parent.length >= dir.length) {
				if (!targets.contains(parent))
					targets.push(parent);
				if (parent == dir)
					break;
				parent = Path.directory(parent);
			}
		}
		for (path in targets)
			Platform.setModificationTime(path, time);
	}

	/**
		Compiles the warm project again without any change, until the watches of
		the virtual file system are active (a few compilations), and returns the
		number of file system calls of the last compilation (`server/vfs`).
	**/
	/** The modules of the case (files of [dir]) that the previous compilation of the server typed. **/
	@:coroutine static function retypedModules(dir:String, port:Int):Array<String> {
		var request = haxe.Json.stringify({jsonrpc: "2.0", id: 1, method: "server/retyped", params: {}});
		var out = Proc.run(haxeCmd, ["--connect", Std.string(port), "--display", request]).stderr();
		var modules:Array<String> = try haxe.Json.parse(out).result.result catch (_) [];
		var l = [];
		for (m in modules)
			if (!l.contains(m) && FileSystem.exists(Path.join([dir, m.split(".").join("/") + ".hx"])))
				l.push(m);
		l.sort(Reflect.compare);
		return l;
	}

	@:coroutine static function noChangeCompilationCalls(dir:String, port:Int):Int {
		var calls = -1;
		for (_ in 0...5) {
			compile(dir, port, false);
			var request = haxe.Json.stringify({jsonrpc: "2.0", id: 1, method: "server/vfs", params: {}});
			var out = Proc.run(haxeCmd, ["--connect", Std.string(port), "--display", request]).stderr();
			calls = try haxe.Json.parse(out).result.result.fileSystemCalls catch (_) -1;
			if (calls == 0)
				break;
		}
		return calls;
	}

	@:coroutine static function sendOverlay(port:Int, file:String, content:Null<String>) {
		var params:Dynamic = {file: file};
		if (content != null)
			params.contents = content;
		var request = haxe.Json.stringify({jsonrpc: "2.0", id: 1, method: "server/overlay", params: params});
		Proc.run(haxeCmd, ["--connect", Std.string(port), "--display", request]);
	}

	@:coroutine static function notifyInvalidate(port:Int, dir:String, file:String) {
		var request = haxe.Json.stringify({
			jsonrpc: "2.0",
			id: 1,
			method: "server/invalidate",
			params: {file: Path.join([dir, file])}
		});
		Proc.run(haxeCmd, ["--connect", Std.string(port), "--cwd", dir, "--display", request]);
	}

	/** The fingerprints of a dump of `-D dump-fingerprints`: `key\tnode` to fingerprint. **/
	public static function readFingerprints(file:String):Map<String, String> {
		var m = new Map();
		for (line in File.getContent(file).split("\n")) {
			var parts = line.split("\t");
			if (parts.length == 3)
				m[parts[0] + "\t" + parts[1]] = parts[2];
		}
		return m;
	}

	/** The dependencies of a dump of `-D dump-fingerprints`: `key\tnode` to the `key\tnode` it read. **/
	public static function readDependencies(file:String):Map<String, Array<String>> {
		var m = new Map<String, Array<String>>();
		for (line in File.getContent(file).split("\n")) {
			var parts = line.split("\t");
			if (parts.length == 5 && parts[2] == "dep") {
				var k = parts[0] + "\t" + parts[1];
				var l = m[k];
				if (l == null)
					m[k] = l = [];
				l.push(parts[3] + "\t" + parts[4]);
			}
		}
		return m;
	}

	/**
		The nodes whose fingerprint changed from [before] to [after] although none
		of the nodes they read did (`--check-deps`, docs/incremental-compilation.md,
		phase 5): a dependency that is not recorded.
	**/
	public static function missingDependencies(before:Map<String, String>, deps:Map<String, Array<String>>, after:Map<String, String>):Array<String> {
		var errors = [];
		// the node that computes [k]: itself, the body of a field for its inline
		// body, the members of its type for a declared signature
		function computedBy(k:String):Null<String> {
			// (recursive local function)
			if (deps.exists(k))
				return k;
			var parts = k.split("\t");
			var key = parts[0], node = parts[1];
			// the messages of a node are computed with it
			if (node.endsWith(" messages"))
				return computedBy(key + "\t" + node.substr(0, node.length - " messages".length));
			switch (node) {
				case "inline":
					for (n in ["body", "sig"])
						if (deps.exists(key + "\t" + n))
							return key + "\t" + n;
				case "sig":
					var paren = key.lastIndexOf(" (");
					if (paren < 0)
						return null;
					var base = key.substr(0, paren);
					var type = key.endsWith("(constructor)") ? base : base.substr(0, base.lastIndexOf("."));
					if (deps.exists(key + "\tdeclared sig"))
						return key + "\tdeclared sig";
					if (deps.exists(type + "\tmembers"))
						return type + "\tmembers";
				case _:
			}
			return null;
		}
		var keys = [for (k in before.keys()) k];
		keys.sort(Reflect.compare);
		for (k in keys) {
			var b = before[k], a = after[k];
			if (b == null || a == null || a == b)
				continue;
			var by = computedBy(k);
			if (by == null)
				continue;
			var inputs = deps[by];
			// a node without a fingerprint (a macro expansion) counts as changed
			var changed = Lambda.exists(inputs, d -> before[d] == null || before[d] != after[d]);
			if (!changed)
				errors.push('missing dependency: ${k.replace("\t", " ")} changed, none of what ${by.replace("\t", " ")} read did (${inputs.length} nodes)');
		}
		return errors;
	}

	/** The fingerprint of [node] of [key], or of a declaration whose file ends with [key]'s. **/
	static function findFingerprint(m:Map<String, String>, node:String, key:String):Null<String> {
		var fp = m[key + "\t" + node];
		if (fp != null)
			return fp;
		for (k => v in m)
			if (k.endsWith("/" + key + "\t" + node) || k.endsWith("\\" + key + "\t" + node))
				return v;
		return null;
	}

	@:coroutine public static function compile(dir:String, port:Null<Int>, clean = true, ?extra:Array<String>):CompileResult {
		var outDir = Path.join([dir, "out"]);
		if (clean) {
			removeDir(outDir);
			FileSystem.createDirectory(outDir);
		}
		var args = port == null ? [] : ["--connect", Std.string(port)];
		args = args.concat(["--cwd", dir, "build.hxml"]);
		if (extra != null)
			args = args.concat(extra);
		var p = Proc.run(haxeCmd, args);
		var code = p.exitCode();
		var out = normalize(p.stdout() + p.stderr(), dir);
		var run = "";
		if (runProgram && code == 0) {
			var js = Path.join([dir, "out", "main.js"]);
			var hl = Path.join([dir, "out", "main.hl"]);
			var cmd = if (FileSystem.exists(js)) ["node", js] else if (FileSystem.exists(hl) && hasHl()) ["hl", hl] else null;
			if (cmd != null) {
				var proc = Proc.run(cmd[0], cmd.slice(1));
				run = normalize(proc.stdout() + proc.stderr(), dir);
			}
		}
		return {
			code: code,
			out: out,
			run: run,
			files: listFiles(outDir, "")
		};
	}

	static var hlAvailable:Null<Bool>;

	/** Whether the HashLink VM is available to run `out/main.hl`. **/
	@:coroutine static function hasHl():Bool {
		if (hlAvailable == null)
			hlAvailable = try {
				Proc.run("hl", ["--version"]);
				true;
			} catch (_:haxe.Exception) false;
		return hlAvailable;
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

	static function withoutDir(s:String, dir:String) {
		return s.split(dir).join("<dir>").split(dir.split("/").join("\\")).join("<dir>");
	}

	public static function compare(warm:CompileResult, cold:CompileResult, warmDir:String, coldDir:String):Array<String> {
		var errors = [];
		if (warm.code != cold.code)
			errors.push('exit code differs: warm ${warm.code}, cold ${cold.code}');
		// the paths of the files of the case in the messages, with either separator (Windows)
		var wl = lines(withoutDir(warm.out, warmDir)), cl = lines(withoutDir(cold.out, coldDir));
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
					// (whether hl is available is known from `detectFeatures`)
					if (target == "interp" || target == "js" || (target == "hl" && hlAvailable)) {
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
				case FingerprintSame(_) | FingerprintChanged(_):
					// Checked on the cold compilation, see runCase.
				case Retyped(_):
					// Checked after the warm compilation, see runCase.
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
	Skipped(reason:String);
	Failed(errors:Array<String>);
	KnownFailure(reason:String, errors:Array<String>);
	UnexpectedPass;
}
