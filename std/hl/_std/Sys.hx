/*
 * Copyright (C)2005-2017 Haxe Foundation
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
class SysError {
	public var msg : String;
	public function new(msg) {
		this.msg = msg;
	}
	@:keep public function toString() {
		return "SysError("+msg+")";
	}
}

@:coreApi
@:keepInit
@:access(String)
class Sys {

	static var utf8Path : Bool;
	static function __init__() : Void {
		utf8Path = sys_utf8_path();
	}
	static function getPath( s : String ) : hl.Bytes {
		return utf8Path ? s.bytes.utf16ToUtf8(0, null) : s.bytes;
	}
	static function makePath( b : hl.Bytes ) : String {
		if( b == null ) return null;
		return utf8Path ? String.fromUTF8(b) : String.fromUCS2(b);
	}

	public static function print( v : Dynamic ) : Void {
		sys_print(Std.string(v).bytes);
	}

	public static function println( v : Dynamic ) : Void {
		sys_print(Std.string(v).bytes);
		sys_print("\n".bytes);
	}

	public static function args() : Array<String> {
		return [for( a in sys_args() ) makePath(a)];
	}

	public static function stdin() : haxe.io.Input {
		return @:privateAccess new sys.io.FileInput(file_stdin());
	}

	public static function stdout() : haxe.io.Output {
		return @:privateAccess new sys.io.FileOutput(file_stdout());
	}

	public static function stderr() : haxe.io.Output {
		return @:privateAccess new sys.io.FileOutput(file_stderr());
	}

	public static function getEnv( s : String ) : String {
		var v = get_env(getPath(s));
		if( v == null )
			return null;
		return makePath(v);
	}

	public static function putEnv( s : String, v : String ) : Void {
		if( !put_env(getPath(s), if( v == null ) null else getPath(v)) ) throw "putEnv() failure";
	}

	public static function environment() : Map<String,String> {
		var env = sys_env();
		var h = new haxe.ds.StringMap();
		for( i in 0...env.length >> 1 ) {
			var p = i << 1;
			h.set(makePath(env[p]), makePath(env[p + 1]));
		}
		return h;
	}

	@:hlNative("std","sys_sleep")
	public static function sleep( seconds : Float ) : Void {
	}

	public static function setTimeLocale( loc : String ) : Bool {
		return set_time_locale(loc.bytes.utf16ToUtf8(0,null));
	}

	public static function getCwd() : String {
		return makePath(get_cwd());
	}

	public static function setCwd( s : String ) : Void {
		if( !set_cwd(getPath(s)) ) throw new SysError("Failed to set path to " + s);
	}

	public static function systemName() : String {
		return String.fromUCS2(sys_string());
	}

	public static function command( cmd : String, ?args : Array<String> ) : Int {
		var code = 0;
		if (args == null) {
			code = sys_command(getPath(cmd));
		} else {
			switch (systemName()) {
				case "Windows":
					cmd = [
						for (a in [StringTools.replace(cmd, "/", "\\")].concat(args))
						StringTools.quoteWinArg(a, true)
					].join(" ");
					code = sys_command(getPath(cmd));
				case _:
					cmd = [cmd].concat(args).map(StringTools.quoteUnixArg).join(" ");
					code = sys_command(getPath(cmd));
			}
		}
		return code;
	}

	@:deprecated("Use programPath instead") public static function executablePath() : String {
		return makePath(sys_exe_path());
	}

	public static function programPath() : String {
		return sys_program_path;
	}
	private static var sys_program_path = {
		var hlFile = sys_hl_file();
		if( hlFile == null )
			makePath( sys_exe_path() );
		else
			sys.FileSystem.fullPath( makePath(hlFile) );
	}

	@:hlNative("std", "sys_utf8_path") static function sys_utf8_path() : Bool { return false; }

	@:hlNative("std","sys_time") public static function time() : Float { return 0.; };
	@:hlNative("std","sys_exit") public static function exit( code : Int ) : Void {};
	@:hlNative("std", "sys_cpu_time") public static function cpuTime() : Float { return 0.; };
	@:hlNative("std", "sys_get_char") public static function getChar( echo : Bool ) : Int { return 0; }

	@:hlNative("std","sys_print") static function sys_print( v : hl.Bytes ) : Void {};
	@:hlNative("std", "file_stdin") static function file_stdin() : sys.io.File.FileHandle { return null; }
	@:hlNative("std", "file_stdout") static function file_stdout() : sys.io.File.FileHandle { return null; }
	@:hlNative("std", "file_stderr") static function file_stderr() : sys.io.File.FileHandle { return null; }
	@:hlNative("std", "sys_args") static function sys_args() : hl.NativeArray<hl.Bytes> { return null; }
	@:hlNative("std", "sys_get_env") static function get_env( key : hl.Bytes ) : hl.Bytes { return null; }
	@:hlNative("std", "sys_put_env") static function put_env( key : hl.Bytes, val : hl.Bytes ) : Bool { return false; }
	@:hlNative("std", "sys_env") static function sys_env() : hl.NativeArray<hl.Bytes> { return null; }
	@:hlNative("std", "sys_set_time_locale") static function set_time_locale( loc : hl.Bytes ) : Bool { return true; }
	@:hlNative("std", "sys_get_cwd") static function get_cwd() : hl.Bytes { return null; }
	@:hlNative("std", "sys_set_cwd") static function set_cwd( path : hl.Bytes ) : Bool { return true; }
	@:hlNative("std", "sys_command") static function sys_command( cmd : hl.Bytes ) : Int { return 0; }
	@:hlNative("std", "sys_exe_path") static function sys_exe_path() : hl.Bytes { return null; }
	@:hlNative("std", "sys_hl_file") static function sys_hl_file() : hl.Bytes { return null; }
	@:hlNative("std", "sys_string") static function sys_string() : hl.Bytes { return null; }

}