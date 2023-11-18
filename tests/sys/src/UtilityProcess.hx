/**
	Used by TestUnicode and TestSys.
	Runs a given simple program based on the first argument.
 */

import haxe.io.Path;
import sys.io.Process;

class UtilityProcess {
	public static var BIN_PATH =
#if cpp
		Path.join(["bin", "cpp"]);
#elseif cs
		Path.join(["bin", "cs", "bin"]);
#elseif hl
	#if hlc
		Path.join(["bin", "hlc/utilityProcess"]);
	#else
		Path.join(["bin", "hl"]);
	#end
#elseif lua
		Path.join(["bin", "lua"]);
#elseif jvm
		Path.join(["bin", "jvm"]);
#elseif java
		Path.join(["bin", "java"]);
#elseif neko
		Path.join(["bin", "neko"]);
#elseif php
		Path.join(["bin", "php"]);
#elseif python
		Path.join(["bin", "python"]);
#elseif eval
		Path.join(["src"]);
#elseif js
		Path.join(["bin", "js"]);
#else
		null;
#end
	public static var BIN_NAME =
#if cpp
		#if debug
			"UtilityProcess-debug";
		#else
			"UtilityProcess";
		#end
#elseif cs
		#if debug
			"UtilityProcess-Debug.exe";
		#else
			"UtilityProcess.exe";
		#end
#elseif hl
	#if hlc
		"UtilityProcess.exe";
	#else
		"UtilityProcess.hl";
	#end
#elseif lua
		"UtilityProcess.lua";
#elseif jvm
		"UtilityProcess.jar";
#elseif java
		#if debug
			"UtilityProcess-Debug.jar";
		#else
			"UtilityProcess.jar";
		#end
#elseif neko
		"UtilityProcess.n";
#elseif php
		Path.join(["UtilityProcess", "index.php"]);
#elseif python
		"UtilityProcess.py";
#elseif eval
		"UtilityProcess.hx";
#elseif js
		"UtilityProcess.js";
#else
		null;
#end

	public static function runUtility(args:Array<String>, ?options:{?stdin:String, ?execPath:String, ?execName:String}):{
		exitCode:Int,
		stdout:String,
		stderr:String
	} {
		if (options == null) options = {};
		if (options.execPath == null) options.execPath = BIN_PATH;
		if (options.execName == null) options.execName = BIN_NAME;
		var execFull = Path.join([options.execPath, options.execName]);
		var proc =
		#if (macro || interp)
		new Process("haxe", ["compile-each.hxml", "-p", options.execPath, "--run", options.execName].concat(args));
		#elseif cpp
		new Process(execFull, args);
		#elseif cs
		(switch (Sys.systemName()) {
			case "Windows":
				new Process(execFull, args);
			case _:
				new Process("mono", [execFull].concat(args));
		});
		#elseif java
		new Process(Path.join([java.lang.System.getProperty("java.home"), "bin", "java"]), ["-jar", execFull].concat(args));
		#elseif python
		new Process(python.lib.Sys.executable, [execFull].concat(args));
		#elseif neko
		new Process("neko", [execFull].concat(args));
		#elseif hl
			#if hlc
			new Process(execFull, args);
			#else
			new Process("hl", [execFull].concat(args));
			#end
		#elseif php
		new Process(php.Global.defined('PHP_BINARY') ? php.Const.PHP_BINARY : 'php', [execFull].concat(args));
		#elseif lua
		new Process("lua", [execFull].concat(args));
		#else
		null;
		#end
		if (options.stdin != null) {
			proc.stdin.writeString(options.stdin);
			proc.stdin.flush();
		}
		var exitCode = proc.exitCode();
		var stdout = proc.stdout.readAll().toString();
		var stderr = proc.stderr.readAll().toString();
		proc.close();
		return {
			exitCode: exitCode,
			stdout: stdout,
			stderr: stderr
		};
	}

	/** Runs the utility program via Sys.command rather than as a separate process,
		for compatiblity with hxnodejs.

		Returns the exit code of the command.
	 **/
	public static function runUtilityAsCommand(args:Array<String>, ?options:{?stdin:String, ?execPath:String, ?execName:String}):Int {
		if (options == null) options = {};
		if (options.execPath == null) options.execPath = BIN_PATH;
		if (options.execName == null) options.execName = BIN_NAME;
		final execFull = Path.join([options.execPath, options.execName]);
		final exitCode =
		#if (macro || interp)
		Sys.command("haxe", ["compile-each.hxml", "-p", options.execPath, "--run", options.execName].concat(args));
		#elseif cpp
		Sys.command(execFull, args);
		#elseif cs
		(switch (Sys.systemName()) {
			case "Windows":
				Sys.command(execFull, args);
			case _:
				Sys.command("mono", [execFull].concat(args));
		});
		#elseif java
		Sys.command(Path.join([java.lang.System.getProperty("java.home"), "bin", "java"]), ["-jar", execFull].concat(args));
		#elseif python
		Sys.command(python.lib.Sys.executable, [execFull].concat(args));
		#elseif neko
		Sys.command("neko", [execFull].concat(args));
		#elseif hl
		Sys.command("hl", [execFull].concat(args));
		#elseif php
		Sys.command(php.Global.defined('PHP_BINARY') ? php.Const.PHP_BINARY : 'php', [execFull].concat(args));
		#elseif lua
		Sys.command("lua", [execFull].concat(args));
		#elseif js
		Sys.command("node", [execFull].concat(args));
		#else
		1;
		#end

		return exitCode;
	}

	public static function main():Void {
		var args = Sys.args();
		function sequenceIndex(d:String, mode:String):String
			return switch UnicodeSequences.valid[Std.parseInt(d)] {
				case Only(ref): UnicodeSequences.codepointsToString(ref);
				case Normal(nfc, nfd): UnicodeSequences.codepointsToString(mode == "nfc" ? nfc : nfd);
			};
		switch (args) {
			case _.slice(0, 1) => ["putEnv"]:
				// ["putEnv", var name, index, nfc mode, next args...]
				Sys.putEnv(args[1], sequenceIndex(args[2], args[3]));
				var out = runUtility(args.slice(4));
				Sys.print(out.stdout);
				Sys.exit(out.exitCode);
			case ["getCwd"]: Sys.println(Sys.getCwd());
			case ["getEnv", name]: Sys.println(Sys.getEnv(name));
			case ["checkEnv", name, value]:
				Sys.exit(value == Sys.getEnv(name) ? 0 : 1);
			case ["environment", name]: Sys.println(Sys.environment().get(name));
			case ["exitCode", Std.parseInt(_) => code]: Sys.exit(code);
			case ["args", data]: Sys.println(data);
			case ["println", d, mode]: Sys.println(sequenceIndex(d, mode));
			case ["print", d, mode]: Sys.print(sequenceIndex(d, mode));
			case ["trace", d, mode]: trace(sequenceIndex(d, mode));
			case ["stdin.readLine"]: Sys.println(Sys.stdin().readLine());
			case ["stdin.readString", Std.parseInt(_) => len]: Sys.println(Sys.stdin().readString(len, UTF8));
			case ["stdin.readUntil", Std.parseInt(_) => end]: Sys.println(Sys.stdin().readUntil(end));
			case ["stderr.writeString", d, mode]:
				var stream = Sys.stderr(); stream.writeString(sequenceIndex(d, mode)); stream.flush();
			case ["stdout.writeString", d, mode]:
				var stream = Sys.stdout(); stream.writeString(sequenceIndex(d, mode)); stream.flush();
			case ["programPath"]: Sys.println(Sys.programPath());
			case _: // no-op
		}
	}
}
