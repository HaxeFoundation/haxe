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
			runCommand("haxe", ["compile-jvm-only.hxml", "--hxb", "bin/hxb/jvm.zip"].concat(args));
			verifyDex("bin/unit.jar");
			runCommand("java", ["-jar", "bin/unit.jar"]);

			runCommand("haxe", ["compile-jvm-only.hxml", "--hxb-lib", "bin/hxb/jvm.zip"].concat(args));
			verifyDex("bin/unit.jar");
			runCommand("java", ["-jar", "bin/unit.jar"]);

			runCommand("haxe", ["compile-jvm-only.hxml","-dce","no"].concat(args));
			verifyDex("bin/unit.jar");
			runCommand("java", ["-jar", "bin/unit.jar"]);

			runCommand("haxe", ["compile-jvm-only.hxml", "--hxb-lib", "bin/hxb/jvm.zip"].concat(args));
			verifyDex("bin/unit.jar");
			runCommand("java", ["-jar", "bin/unit.jar"]);
		}

		changeDirectory(getMiscSubDir(""));
		runCommand("haxe", ["run-base.hxml", "--run", "Main", "jvm"]);
		verifyDexAll(miscJvmProjectJars());

		changeDirectory(sysDir);
		runCommand("haxe", args.concat(["compile-jvm.hxml"]));
		verifyDex("bin/jvm/sys.jar");
		runSysTest("java", ["-jar", "bin/jvm/sys.jar"]);

		changeDirectory(threadsDir);
		runCommand("haxe", ["build.hxml", "--jvm", "export/threads.jar"].concat(args));
		verifyDex("export/threads.jar");
		runCommand("java", ["-jar", "export/threads.jar"]);
	}
}
