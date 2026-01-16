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

@:coreApi class Sys {
	private static var _args:Array<String>;
	private static var _env:haxe.ds.StringMap<String>;
	private static var _sysName:String;

	public static function print(v:Dynamic):Void {
		untyped __cs__("System.Console.Write({0})", Std.string(v));
	}

	public static function println(v:Dynamic):Void {
		untyped __cs__("System.Console.WriteLine({0})", Std.string(v));
	}

	public static function args():Array<String> {
		if (_args == null)
			return [];
		return _args.copy();
	}

	public static function getEnv(s:String):Null<String> {
		return untyped __cs__("System.Environment.GetEnvironmentVariable({0})", s);
	}

	public static function putEnv(s:String, v:Null<String>):Void {
		untyped __cs__("System.Environment.SetEnvironmentVariable({0}, {1})", s, v);
	}

	public static function environment():Map<String, String> {
		if (_env == null) {
			_env = new haxe.ds.StringMap();
			var dict:Dynamic = untyped __cs__("System.Environment.GetEnvironmentVariables()");
			var enumerator:Dynamic = untyped dict.GetEnumerator();
			while (untyped enumerator.MoveNext()) {
				var entry:Dynamic = untyped enumerator.Current;
				_env.set(untyped entry.Key, untyped entry.Value);
			}
		}
		return _env.copy();
	}

	public static function sleep(seconds:Float):Void {
		untyped __cs__("System.Threading.Thread.Sleep((int)({0} * 1000))", seconds);
	}

	public static function setTimeLocale(loc:String):Bool {
		return false;
	}

	public static function getCwd():String {
		return untyped __cs__("System.IO.Directory.GetCurrentDirectory()");
	}

	public static function setCwd(s:String):Void {
		untyped __cs__("System.IO.Directory.SetCurrentDirectory({0})", s);
	}

	public static function systemName():String {
		if (_sysName != null)
			return _sysName;
		var platform:Dynamic = untyped __cs__("System.Environment.OSVersion.Platform");
		var platformId:Int = untyped __cs__("(int){0}", platform);
		// PlatformID enum: Win32NT=2, Unix=4, MacOSX=6
		if (platformId == 2)
			return _sysName = "Windows";
		if (platformId == 6)
			return _sysName = "Mac";
		if (platformId == 4)
			return _sysName = "Linux";
		return _sysName = "Unknown";
	}

	public static function command(cmd:String, ?args:Array<String>):Int {
		var process:Dynamic = untyped __cs__("new System.Diagnostics.Process()");
		untyped process.StartInfo.FileName = cmd;
		if (args != null) {
			untyped process.StartInfo.Arguments = args.join(" ");
		}
		untyped process.StartInfo.UseShellExecute = false;
		untyped process.StartInfo.RedirectStandardOutput = true;
		untyped process.StartInfo.RedirectStandardError = true;
		untyped process.Start();
		untyped process.WaitForExit();
		return untyped process.ExitCode;
	}

	public static function exit(code:Int):Void {
		untyped __cs__("System.Environment.Exit({0})", code);
	}

	public static function time():Float {
		var ticks:Float = untyped __cs__("(double)System.DateTime.UtcNow.Ticks");
		// Ticks are 100-nanosecond intervals since Jan 1, 0001
		// Convert to seconds since Unix epoch (Jan 1, 1970)
		var ticksPerSecond:Float = 10000000.0;
		var epochTicks:Float = untyped __cs__("(double)new System.DateTime(1970, 1, 1, 0, 0, 0, System.DateTimeKind.Utc).Ticks");
		return (ticks - epochTicks) / ticksPerSecond;
	}

	public static function cpuTime():Float {
		var ticks:Float = untyped __cs__("(double)System.Diagnostics.Stopwatch.GetTimestamp()");
		var freq:Float = untyped __cs__("(double)System.Diagnostics.Stopwatch.Frequency");
		return ticks / freq;
	}

	@:deprecated("Use programPath instead")
	public static function executablePath():String {
		return getCwd();
	}

	public static function programPath():String {
		return untyped __cs__("System.Reflection.Assembly.GetExecutingAssembly().Location");
	}

	public static function getChar(echo:Bool):Int {
		var key:Dynamic = untyped __cs__("System.Console.ReadKey({0})", !echo);
		return untyped __cs__("(int){0}.KeyChar", key);
	}

	public static function stdin():haxe.io.Input {
		// TODO: implement properly
		throw new haxe.exceptions.NotImplementedException();
	}

	public static function stdout():haxe.io.Output {
		// TODO: implement properly
		throw new haxe.exceptions.NotImplementedException();
	}

	public static function stderr():haxe.io.Output {
		// TODO: implement properly
		throw new haxe.exceptions.NotImplementedException();
	}
}
