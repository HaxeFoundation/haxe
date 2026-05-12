package runci.targets;

import sys.FileSystem;
import haxe.io.Path;
import runci.System.*;
import runci.Config.*;

class Jvm {
	static inline final VERIFIER_DIR = "misc/jvm/dex";

	static public function getJavaDependencies() {
		haxelibInstallGit("HaxeFoundation", "hxjava", true);
		haxelibInstallGit("HaxeFoundation", "format", "jvm", "--always");
		runCommand("javac", ["-version"]);
	}

	static function buildAndRun(args:Array<String>, output:String, ?run:(String, Array<String>)->Void):Void {
		final run = run ?? runCommand;
		runCommand("haxe", args);
		run("java", ["-jar", output]);
	}

	// Rebuilds with `-D jvm.dex-compatible`, runs the resulting jar (to catch
	// behavior regressions from the dex codegen path) and runs d8 against it
	// (to catch dex-rejection regressions). Called once per target rather
	// than per build variant — the flag's user-visible effect doesn't change
	// across dynamic-level / hxb / dce, so one representative config gives
	// us the regression signal at ~10% of the wall-clock cost.
	static function checkDexCompatible(args:Array<String>, output:String, ?run:(String, Array<String>)->Void):Void {
		final run = run ?? runCommand;
		runCommand("haxe", args.concat(["-D", "jvm.dex-compatible"]));
		run("java", ["-jar", output]);
		verifyDex(output);
	}

	// Hard-fail when d8 should be present, soft-skip on Windows where the
	// Android SDK isn't part of the standard runner image. Local devs always
	// get the soft path so a missing SDK doesn't break their builds.
	static function dexHardFail():Bool {
		return isCi() && systemName != "Windows";
	}

	// Runs d8 against `jar` and fails the build on any error or non-allowlisted
	// warning. Behavior on missing d8 is governed by `dexHardFail`.
	static function verifyDex(jar:String) {
		final prevCwd = Sys.getCwd();
		Sys.setCwd(Path.join([cwd, VERIFIER_DIR]));
		final extra = dexHardFail() ? [] : ["--soft"];
		runCommand("haxe", ["verify-all.hxml", "--run", "VerifyAllJars"].concat(extra).concat([Path.join([prevCwd, jar])]));
		Sys.setCwd(prevCwd);
	}

	static function verifyDexAll(jars:Array<String>) {
		if (jars.length == 0) return;
		final prevCwd = Sys.getCwd();
		Sys.setCwd(Path.join([cwd, VERIFIER_DIR]));
		final extra = dexHardFail() ? [] : ["--soft"];
		final abs = jars.map(j -> Path.isAbsolute(j) ? j : Path.join([prevCwd, j]));
		runCommand("haxe", ["verify-all.hxml", "--run", "VerifyAllJars"].concat(extra).concat(abs));
		Sys.setCwd(prevCwd);
	}

	static function miscJvmProjectJars():Array<String> {
		final base = getMiscSubDir("jvm", "projects");
		if (!FileSystem.exists(base)) return [];
		final out = [];
		for (entry in FileSystem.readDirectory(base)) {
			final jar = Path.join([base, entry, "bin", "run.jar"]);
			if (FileSystem.exists(jar)) out.push(jar);
		}
		return out;
	}

	static public function run(args:Array<String>) {
		deleteDirectoryRecursively("bin/jvm");
		getJavaDependencies();

		runCommand("haxe", ["compile-java-native.hxml"]);

		for (level in 0...3) {
			final args = args.concat(["-D", "jvm.dynamic-level=" + level]);
			buildAndRun(["compile-jvm-only.hxml", "--hxb", "bin/hxb/jvm.zip"].concat(args), "bin/unit.jar");
			buildAndRun(["compile-jvm-only.hxml", "--hxb-lib", "bin/hxb/jvm.zip"].concat(args), "bin/unit.jar");
			buildAndRun(["compile-jvm-only.hxml","-dce","no"].concat(args), "bin/unit.jar");
			buildAndRun(["compile-jvm-only.hxml", "--hxb-lib", "bin/hxb/jvm.zip"].concat(args), "bin/unit.jar");
		}
		// One dex-compatible pass for the unit suite at the most codegen-heavy
		// config (level=2 + --hxb — the variant the original regression came from).
		checkDexCompatible(["compile-jvm-only.hxml", "--hxb", "bin/hxb/jvm.zip", "-D", "jvm.dynamic-level=2"].concat(args), "bin/unit.jar");

		changeDirectory(getMiscSubDir(""));
		runCommand("haxe", ["run-base.hxml", "--run", "Main", "jvm"]);
		verifyDexAll(miscJvmProjectJars());

		changeDirectory(sysDir);
		buildAndRun(args.concat(["compile-jvm.hxml"]), "bin/jvm/sys.jar", runSysTest);
		checkDexCompatible(args.concat(["compile-jvm.hxml"]), "bin/jvm/sys.jar", runSysTest);

		changeDirectory(threadsDir);
		buildAndRun(["build.hxml", "--jvm", "export/threads.jar"].concat(args), "export/threads.jar");
		checkDexCompatible(["build.hxml", "--jvm", "export/threads.jar"].concat(args), "export/threads.jar");
	}
}
