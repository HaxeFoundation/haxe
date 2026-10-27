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

		for (level in 0...2) {
			final args = args.concat(["-D", "jvm.dynamic-level=" + level]);
			buildAndRun(["compile-jvm-only.hxml", "--hxb", "bin/hxb/jvm.zip"].concat(args), "bin/unit.jar");
			buildAndRun(["compile-jvm-only.hxml", "--hxb-lib", "bin/hxb/jvm.zip"].concat(args), "bin/unit.jar");
			buildAndRun(["compile-jvm-only.hxml","-dce","no"].concat(args), "bin/unit.jar");
			buildAndRun(["compile-jvm-only.hxml", "--hxb-lib", "bin/hxb/jvm.zip"].concat(args), "bin/unit.jar");
		}
		// One dex check on the existing unit jar (codegen path is the same
		// across all build variants — running d8 once gives the regression
		// signal without rebuilding).
		verifyDex("bin/unit.jar");

		changeDirectory(getMiscSubDir(""));
		runCommand("haxe", ["run-base.hxml", "--run", "Main", "jvm"]);
		verifyDexAll(miscJvmProjectJars());

		changeDirectory(sysDir);
		buildAndRun(args.concat(["compile-jvm.hxml"]), "bin/jvm/sys.jar", runSysTest);
		verifyDex("bin/jvm/sys.jar");

		changeDirectory(threadsDir);
		buildAndRun(["build.hxml", "--jvm", "export/threads.jar"].concat(args), "export/threads.jar");
		verifyDex("export/threads.jar");
	}
}
