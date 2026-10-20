import haxe.io.Path;
import sys.FileSystem;
import sys.io.File;
import sys.io.Process;

using StringTools;

/**
	History replay: compiles every commit of a range of a git repository, warm
	(through a compilation server that compiled the previous commits) and cold
	(fresh compiler), and compares the results like the scenario runner does
	(exit code, messages, program output, generated files).

	Usage (from `tests/incremental`):
	  haxe replay.hxml -- --repo <path> --hxml <file> [options]
	Options:
	  --repo <path>         local git repository (its history must contain the range)
	  --hxml <file>         build file; `$DIR` is replaced with the checkout directory,
	                        outputs must go to `out/`
	  --commits <n>         replay the last n commits of HEAD (default 10)
	  --range <a..b>        replay the commits of a git range instead
	  --haxe <path>         compiler to test (default: `haxe` from PATH)
	  --run                 run `out/main.js` with node and compare its output
	  --keep                keep the work directory
	  --ignore-order        ignore differences of line order in generated files
**/
class Replay {
	static function main() {
		var args = Sys.args();
		var repo:Null<String> = null;
		var hxml:Null<String> = null;
		var commits = 10;
		var range:Null<String> = null;
		var keep = false;
		RunIncremental.runProgram = false;
		var i = 0;
		while (i < args.length) {
			switch (args[i]) {
				case "--repo": repo = FileSystem.fullPath(args[++i]);
				case "--hxml": hxml = File.getContent(args[++i]);
				case "--commits": commits = Std.parseInt(args[++i]);
				case "--range": range = args[++i];
				case "--haxe": RunIncremental.haxeCmd = args[++i];
				case "--run": RunIncremental.runProgram = true;
				case "--keep": keep = true;
				case "--ignore-order": RunIncremental.ignoreOrder = true;
				case other:
					Sys.println('Unknown option $other');
					Sys.exit(2);
			}
			i++;
		}
		if (repo == null || hxml == null) {
			Sys.println("--repo and --hxml are required");
			Sys.exit(2);
		}

		var list = git(repo, ["rev-list", "--reverse", "--first-parent", range != null ? range : 'HEAD~$commits..HEAD']).split("\n").filter(s -> s != "");
		var work = Path.join([Sys.getCwd(), "work", "replay"]);
		RunIncremental.removeDir(work);
		FileSystem.createDirectory(work);
		var warmDir = Path.join([work, "warm"]);
		var coldDir = Path.join([work, "cold"]);
		for (dir in [warmDir, coldDir])
			git(work, ["clone", "--quiet", "--no-checkout", repo, dir]);

		var port = 7900 + Std.random(90);
		var server = new Process(RunIncremental.haxeCmd, ["--wait", Std.string(port)]);
		Sys.sleep(1);

		var failures = 0;
		var warmTime = 0.;
		var coldTime = 0.;
		Sys.println('Replaying ${list.length} commits of $repo');
		for (commit in list) {
			for (dir in [warmDir, coldDir]) {
				git(dir, ["checkout", "--quiet", "--force", commit]);
				git(dir, ["clean", "--quiet", "-fdx", "-e", "out", "-e", "build.hxml"]);
				File.saveContent(Path.join([dir, "build.hxml"]), hxml.replace("$DIR", dir));
			}
			var t0 = Sys.time();
			var warm = RunIncremental.compile(warmDir, port);
			var t1 = Sys.time();
			var cold = RunIncremental.compile(coldDir, null);
			var t2 = Sys.time();
			warmTime += t1 - t0;
			coldTime += t2 - t1;
			var errors = RunIncremental.compare(warm, cold, warmDir, coldDir);
			var subject = git(warmDir, ["log", "-1", "--format=%s", commit]).trim();
			var summary = '${commit.substr(0, 10)} warm ${Math.round((t1 - t0) * 1000)}ms cold ${Math.round((t2 - t1) * 1000)}ms exit ${warm.code}';
			if (errors.length == 0) {
				Sys.println('SAME   $summary  $subject');
			} else {
				failures++;
				Sys.println('DIFF   $summary  $subject');
				for (e in errors)
					Sys.println("       " + e.split("\n").join("\n       "));
			}
		}
		server.kill();
		server.close();
		Sys.println("");
		Sys.println('${list.length - failures} identical, $failures different; total warm ${Math.round(warmTime * 1000)}ms, cold ${Math.round(coldTime * 1000)}ms');
		if (!keep)
			RunIncremental.removeDir(work);
		Sys.exit(failures > 0 ? 1 : 0);
	}

	static function git(dir:String, args:Array<String>):String {
		var p = new Process("git", ["-C", dir].concat(args));
		var out = p.stdout.readAll().toString();
		var err = p.stderr.readAll().toString();
		var code = p.exitCode();
		p.close();
		if (code != 0)
			throw 'git ${args.join(" ")} failed: $err';
		return out;
	}
}
