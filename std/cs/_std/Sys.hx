/*
 * Copyright (C)2005-2019 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

import cs.system.Console;
import cs.system.Environment;
import cs.system.io.Directory;
import cs.system.threading.Thread;
import cs.system.DateTime;
import cs.system.TimeSpan;
import cs.system.diagnostics.Process;
import cs.system.reflection.Assembly;

@:coreApi class Sys {
	private static var _args:Array<String>;
	private static var _sysName:String;
	private static var _removedEnvVars:haxe.ds.StringMap<Bool>;

	public static function print(v:Dynamic):Void {
		var str = Std.string(v);
		// On Windows, Console.Write doesn't translate embedded \n to \r\n,
		// causing staircase output in some terminals. Normalize to Environment.NewLine.
		var nl:String = untyped __cs__("System.Environment.NewLine");
		if (nl != "\n") {
			str = untyped __cs__("{0}.Replace(\"\\r\\n\", \"\\n\").Replace(\"\\n\", {1})", str, nl);
		}
		Console.Write(str);
	}

	public static function println(v:Dynamic):Void {
		var str = Std.string(v);
		var nl:String = untyped __cs__("System.Environment.NewLine");
		if (nl != "\n") {
			str = untyped __cs__("{0}.Replace(\"\\r\\n\", \"\\n\").Replace(\"\\n\", {1})", str, nl);
		}
		Console.WriteLine(str);
	}

	public static function args():Array<String> {
		if (_args == null) {
			var nativeArgs = Environment.GetCommandLineArgs();
			_args = new Array<String>();
			// Skip first arg (executable path)
			for (i in 1...nativeArgs.length) {
				_args.push(nativeArgs[i]);
			}
		}
		return _args.copy();
	}

	public static function getEnv(s:String):Null<String> {
		return Environment.GetEnvironmentVariable(s);
	}

	public static function putEnv(s:String, v:Null<String>):Void {
		Environment.SetEnvironmentVariable(s, v);
		// Track removed variables for subprocess environment inheritance in AOT
		if (v == null) {
			if (_removedEnvVars == null)
				_removedEnvVars = new haxe.ds.StringMap<Bool>();
			_removedEnvVars.set(s, true);
		} else if (_removedEnvVars != null) {
			_removedEnvVars.remove(s);
		}
	}

	@:allow(sys.io.Process)
	private static function getRemovedEnvVars():Null<haxe.ds.StringMap<Bool>> {
		return _removedEnvVars;
	}

	public static function environment():Map<String, String> {
		var env = new haxe.ds.StringMap<String>();
		var dict = Environment.GetEnvironmentVariables();
		// Use __cs__ foreach to avoid Dynamic property access issues with IDictionaryEnumerator
		untyped __cs__("foreach (System.Collections.DictionaryEntry entry in {0}) {
			{1}.set((string)entry.Key, (string)entry.Value);
		}", dict, env);
		return env;
	}

	public static function sleep(seconds:Float):Void {
		Thread.Sleep(Std.int(seconds * 1000));
	}

	public static function setTimeLocale(loc:String):Bool {
		return false;
	}

	public static function getCwd():String {
		return haxe.io.Path.addTrailingSlash(Directory.GetCurrentDirectory());
	}

	public static function setCwd(s:String):Void {
		Directory.SetCurrentDirectory(s);
	}

	public static function systemName():String {
		if (_sysName != null)
			return _sysName;
		// Use Dynamic to prevent Haxe from inferring Int type from extern enum abstract
		var platform:Dynamic = Environment.OSVersion.Platform;
		// Use string comparison since enum values might not match across .NET versions
		var platformStr = Std.string(platform);
		if (platformStr == "Unix")
			return _sysName = "Linux";
		if (platformStr == "MacOSX")
			return _sysName = "Mac";
		if (platformStr == "Xbox")
			return _sysName = "Xbox";
		// For numeric comparison (PlatformID: Win32NT=2, Unix=4, MacOSX=6)
		var platformId:Int = untyped __cs__("(int){0}", platform);
		if (platformId == 4 || platformId == 6 || platformId == 128)
			return _sysName = "Linux";
		return _sysName = "Windows";
	}

	public static function command(cmd:String, ?args:Array<String>):Int {
		var process = new Process();
		process.StartInfo.UseShellExecute = false;
		process.StartInfo.RedirectStandardOutput = true;
		process.StartInfo.RedirectStandardError = true;
		if (args != null) {
			// Use ArgumentList collection (like JVM/Python) - no escaping needed
			process.StartInfo.FileName = cmd;
			var startInfo = process.StartInfo;
			for (arg in args) {
				untyped __cs__("{0}.ArgumentList.Add({1})", startInfo, arg);
			}
		} else {
			// Shell command - use /bin/sh -c (or cmd.exe /C on Windows)
			switch (systemName()) {
				case "Windows":
					process.StartInfo.FileName = switch (getEnv("COMSPEC")) {
						case null: "cmd.exe";
						case comspec: comspec;
					};
					var startInfo = process.StartInfo;
					untyped __cs__("{0}.ArgumentList.Add({1})", startInfo, "/C");
					untyped __cs__("{0}.ArgumentList.Add({1})", startInfo, cmd);
				case _:
					process.StartInfo.FileName = "/bin/sh";
					var startInfo = process.StartInfo;
					untyped __cs__("{0}.ArgumentList.Add({1})", startInfo, "-c");
					untyped __cs__("{0}.ArgumentList.Add({1})", startInfo, cmd);
			}
		}
		process.Start();
		// Read output streams before WaitForExit to avoid deadlock
		var stdoutStream = process.StandardOutput.BaseStream;
		var stderrStream = process.StandardError.BaseStream;
		var stdoutWrapper = new cs.io.NativeInput(stdoutStream);
		var stderrWrapper = new cs.io.NativeInput(stderrStream);
		// Read and print stdout
		try {
			while (true) {
				var line = stdoutWrapper.readLine();
				println(line);
			}
		} catch (e:haxe.io.Eof) {}
		// Read and print stderr to stderr
		var stderrOutput = stderr();
		try {
			while (true) {
				var line = stderrWrapper.readLine();
				stderrOutput.writeString(line + "\n");
			}
		} catch (e:haxe.io.Eof) {}
		process.WaitForExit();
		return process.ExitCode;
	}

	public static function exit(code:Int):Void {
		Environment.Exit(code);
	}

	static var epochTicks:haxe.Int64 = new DateTime(1970, 1, 1).Ticks;

	public static function time():Float {
		// Use __cs__ for reliable Int64 to Float conversion
		var ticks:Float = untyped __cs__("(double){0}", DateTime.UtcNow.Ticks);
		var epoch:Float = untyped __cs__("(double){0}", epochTicks);
		var ticksPerSecond:Float = untyped __cs__("(double){0}", TimeSpan.TicksPerSecond);
		return (ticks - epoch) / ticksPerSecond;
	}

	public static function cpuTime():Float {
		return Environment.TickCount / 1000.0;
	}

	@:deprecated("Use programPath instead")
	public static function executablePath():String {
		return getCwd();
	}

	public static function programPath():String {
		// Try multiple approaches for AOT compatibility
		// 1. Environment.ProcessPath (.NET 6+ only, not in .NET Standard 2.1)
		var path:String = untyped __cs__("
#if !NETSTANDARD
			System.Environment.ProcessPath
#else
			null
#endif
		");
		if (path == null || path == "") {
			// 2. Process.MainModule.FileName (more reliable for AOT)
			path = untyped __cs__("System.Diagnostics.Process.GetCurrentProcess().MainModule?.FileName");
		}
		if (path == null || path == "") {
			// 3. Assembly.Location (fallback for non-AOT)
			path = Assembly.GetExecutingAssembly().Location;
		}
		return path;
	}

	public static function getChar(echo:Bool):Int {
		var keyInfo = Console.ReadKey(!echo);
		return keyInfo.KeyChar;
	}

	public static function stdin():haxe.io.Input {
		return new cs.io.NativeInput(Console.OpenStandardInput());
	}

	public static function stdout():haxe.io.Output {
		return new cs.io.NativeOutput(Console.OpenStandardOutput());
	}

	public static function stderr():haxe.io.Output {
		return new cs.io.NativeOutput(Console.OpenStandardError());
	}
}
