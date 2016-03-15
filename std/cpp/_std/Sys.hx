/*
 * Copyright (C)2005-2016 Haxe Foundation
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

	public static function print( v : Dynamic ) : Void {
		untyped __global__.__hxcpp_print(v);
	}

	public static function println( v : Dynamic ) : Void {
		print(v);
		print("\n");
	}

	public static function stdin() : haxe.io.Input {
		return untyped new sys.io.FileInput(file_stdin());
	}

	public static function stdout() : haxe.io.Output {
		return untyped new sys.io.FileOutput(file_stdout());
	}

	public static function stderr() : haxe.io.Output {
		return untyped new sys.io.FileOutput(file_stderr());
	}

	public static function getChar( echo : Bool ) : Int {
		return getch(echo);
	}

	public static function args() : Array<String> untyped {
		return __global__.__get_args();
	}

	public static function getEnv( s : String ):String {
		var v = get_env(s);
		if( v == null )
			return null;
		return v;
	}

	public static function putEnv( s : String, v : String ) : Void {
		put_env(s,v);
	}

	public static function sleep( seconds : Float ) : Void {
		_sleep(seconds);
	}

	public static function setTimeLocale( loc : String ) : Bool {
		return set_time_locale(loc);
	}

	public static function getCwd() : String {
		return new String(get_cwd());
	}

	public static function setCwd( s : String ) : Void {
		set_cwd(s);
	}

	public static function systemName() : String {
		return sys_string();
	}

	public static function command( cmd : String, ?args : Array<String> ) : Int {
		if (args == null) {
			return sys_command(cmd);
		} else {
			switch (systemName()) {
				case "Windows":
					cmd = [
						for (a in [StringTools.replace(cmd, "/", "\\")].concat(args))
						StringTools.quoteWinArg(a, true)
					].join(" ");
					return sys_command(cmd);
				case _:
					cmd = [cmd].concat(args).map(StringTools.quoteUnixArg).join(" ");
					return sys_command(cmd);
			}
		}
	}

	public static function exit( code : Int ) : Void {
		untyped __global__.__hxcpp_exit(code);
	}

	public static function time() : Float {
		return sys_time();
	}

	public static function cpuTime() : Float {
		return sys_cpu_time();
	}

	@:deprecated("Use programPath instead") public static function executablePath() : String {
		return new String(sys_exe_path());
	}

	public static function programPath() : String {
		return _programPath;
	}

	public static function environment() : Map<String,String> {
		var vars:Array<String> = sys_env();
		var result = new haxe.ds.StringMap<String>();
		var i = 0;
		while(i<vars.length) {
			result.set( vars[i], vars[i+1] );
			i+=2;
		}
		return result;
	}

	private static var get_env = cpp.Lib.load("std","get_env",1);
	private static var put_env = cpp.Lib.load("std","put_env",2);
	private static var _sleep = cpp.Lib.load("std","sys_sleep",1);
	private static var set_time_locale = cpp.Lib.load("std","set_time_locale",1);
	private static var get_cwd = cpp.Lib.load("std","get_cwd",0);
	private static var set_cwd = cpp.Lib.load("std","set_cwd",1);
	private static var sys_string = cpp.Lib.load("std","sys_string",0);
	private static var sys_command = cpp.Lib.load("std","sys_command",1);
	private static var sys_time = cpp.Lib.load("std","sys_time",0);
	private static var sys_cpu_time = cpp.Lib.load("std","sys_cpu_time",0);
	private static var sys_exe_path = cpp.Lib.load("std","sys_exe_path",0);
	private static var _programPath = sys.FileSystem.fullPath(new String(sys_exe_path()));
	private static var sys_env = cpp.Lib.load("std","sys_env",0);

	private static var file_stdin = cpp.Lib.load("std","file_stdin",0);
	private static var file_stdout = cpp.Lib.load("std","file_stdout",0);
	private static var file_stderr = cpp.Lib.load("std","file_stderr",0);

	private static var getch = cpp.Lib.load("std","sys_getch",1);
}
