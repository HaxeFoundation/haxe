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

	static function checkAndRun(args:Array<String>, output:String, ?run:(String, Array<String>)->Void):Void {
		final run = run ?? runCommand;

		runCommand("haxe", args);
		run("java", ["-jar", output]);

		runCommand("haxe", args.concat(["-D", "jvm.dex-compatible"]));
		verifyDex(output);
		run("java", ["-jar", output]);
	}

	// Runs d8 against `jar` and fails the build on any error or non-allowlisted
	// warning. No-op when d8 isn't available (allowed locally via --soft).
	static function verifyDex(jar:String) {
		final prevCwd = Sys.getCwd();
		Sys.setCwd(Path.join([cwd, VERIFIER_DIR]));
		final extra = isCi() ? [] : ["--soft"];
		runCommand("haxe", ["verify-all.hxml", "--run", "VerifyAllJars"].concat(extra).concat([Path.join([prevCwd, jar])]));
		Sys.setCwd(prevCwd);
	}

	static function verifyDexAll(jars:Array<String>) {
		if (jars.length == 0) return;
		final prevCwd = Sys.getCwd();
		Sys.setCwd(Path.join([cwd, VERIFIER_DIR]));
		final extra = isCi() ? [] : ["--soft"];
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
			checkAndRun(["compile-jvm-only.hxml", "--hxb", "bin/hxb/jvm.zip"].concat(args), "bin/unit.jar");
			checkAndRun(["compile-jvm-only.hxml", "--hxb-lib", "bin/hxb/jvm.zip"].concat(args), "bin/unit.jar");
			checkAndRun(["compile-jvm-only.hxml","-dce","no"].concat(args), "bin/unit.jar");
			checkAndRun(["compile-jvm-only.hxml", "--hxb-lib", "bin/hxb/jvm.zip"].concat(args), "bin/unit.jar");
		}

		changeDirectory(getMiscSubDir(""));
		runCommand("haxe", ["run-base.hxml", "--run", "Main", "jvm"]);
		verifyDexAll(miscJvmProjectJars());

		changeDirectory(sysDir);
		checkAndRun(args.concat(["compile-jvm.hxml"]), "bin/jvm/sys.jar", runSysTest);

		changeDirectory(threadsDir);
		checkAndRun(["build.hxml", "--jvm", "export/threads.jar"].concat(args), "export/threads.jar");
	}
}
