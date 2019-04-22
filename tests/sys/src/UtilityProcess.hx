/**
	Used by TestUnicode.
	Runs a given simple program based on the first argument.
 */

class UtilityProcess {
	public static var BIN_PATH =
#if cpp
		"bin/cpp";
#elseif cs
		"bin/cs/bin";
#elseif hl
		"bin/hl";
#elseif java
		"bin/java";
#elseif neko
		"bin/neko";
#elseif php
		"bin/php";
#elseif python
		"bin/python";
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
		"UtilityProcess.hl";
#elseif java
		#if debug
			"UtilityProcess-Debug.jar";
		#else
			"UtilityProcess.jar";
		#end
#elseif neko
		"UtilityProcess.n";
#elseif php
		"UtilityProcess/index.php";
#elseif python
		"UtilityProcess.py";
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
		var execFull = '${options.execPath}/${options.execName}';
		var proc =
		#if (macro || interp)
		new sys.io.Process("haxe", ["compile-each.hxml", "--run", execFull].concat(args));
		#elseif cpp
		new sys.io.Process(execFull, args);
		#elseif cs
		(switch (Sys.systemName()) {
			case "Windows":
				new sys.io.Process(execFull, args);
			case _:
				new sys.io.Process("mono", [execFull].concat(args));
		});
		#elseif java
		new sys.io.Process(haxe.io.Path.join([java.lang.System.getProperty("java.home"), "bin", "java"]), ["-jar", execFull].concat(args));
		#elseif python
		new sys.io.Process(python.lib.Sys.executable, [execFull].concat(args));
		#elseif neko
		new sys.io.Process("neko", [execFull].concat(args));
		#elseif hl
		new sys.io.Process("hl", [execFull].concat(args));
		#elseif php
		new sys.io.Process(php.Global.defined('PHP_BINARY') ? php.Const.PHP_BINARY : 'php', [execFull].concat(args));
		#elseif lua
		new sys.io.Process("lua", [execFull].concat(args));
		#else
		null;
		#end
		if (options.stdin != null) {
			proc.stdin.writeString(options.stdin);
			proc.stdin.flush();
		}
		return {
			exitCode: proc.exitCode(),
			stdout: proc.stdout.readAll().toString(),
			stderr: proc.stderr.readAll().toString()
		};
	}
	
	public static function main():Void {
		var args = Sys.args();
		function sequenceIndex(d:String, mode:String):String return (switch (UnicodeSequences.valid[Std.parseInt(d)]) {
				case Only(ref): UnicodeSequences.codepointsToString(ref);
				case Normal(nfc, nfd): UnicodeSequences.codepointsToString(mode == "nfc" ? nfc : nfd);
			});
		switch (args) {
			case _.slice(0, 1) => ["putEnv"]:
			// ["putEnv", var name, index, nfc mode, next args...]
			Sys.putEnv(args[1], sequenceIndex(args[2], args[3]));
			var out = runUtility(args.slice(4));
			Sys.print(out.stdout);
			Sys.exit(out.exitCode);
			case ["getCwd"]: Sys.println(Sys.getCwd());
			case ["getEnv", name]: Sys.println(Sys.getEnv(name));
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
