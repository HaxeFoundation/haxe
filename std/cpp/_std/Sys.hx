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
import cpp.NativeSys;

@:coreApi class Sys {

	public static function print( v : Dynamic ) : Void {
		untyped __global__.__hxcpp_print(v);
	}

	public static function println( v : Dynamic ) : Void {
		untyped __global__.__hxcpp_println(v);
	}

   @:access(sys.io.FileInput)
	public static function stdin() : haxe.io.Input {
		return new sys.io.FileInput(cpp.NativeFile.file_stdin());
	}

   @:access(sys.io.FileOutput)
	public static function stdout() : haxe.io.Output {
		return new sys.io.FileOutput(cpp.NativeFile.file_stdout());
	}

   @:access(sys.io.FileOutput)
	public static function stderr() : haxe.io.Output {
		return new sys.io.FileOutput(cpp.NativeFile.file_stderr());
	}

	public static function getChar( echo : Bool ) : Int {
		return NativeSys.sys_getch(echo);
	}

	public static function args() : Array<String> untyped {
		return __global__.__get_args();
	}

	public static function getEnv( s : String ):String {
		var v = NativeSys.get_env(s);
		if( v == null )
			return null;
		return v;
	}

	public static function putEnv( s : String, v : String ) : Void {
		NativeSys.put_env(s,v);
	}

	public static function sleep( seconds : Float ) : Void {
		NativeSys.sys_sleep(seconds);
	}

	public static function setTimeLocale( loc : String ) : Bool {
		return NativeSys.set_time_locale(loc);
	}

	public static function getCwd() : String {
		return NativeSys.get_cwd();
	}

	public static function setCwd( s : String ) : Void {
		NativeSys.set_cwd(s);
	}

	public static function systemName() : String {
		return NativeSys.sys_string();
	}

	public static function command( cmd : String, ?args : Array<String> ) : Int {
		if (args == null) {
			return NativeSys.sys_command(cmd);
		} else {
			switch (systemName()) {
				case "Windows":
					cmd = [
						for (a in [StringTools.replace(cmd, "/", "\\")].concat(args))
						StringTools.quoteWinArg(a, true)
					].join(" ");
					return NativeSys.sys_command(cmd);
				case _:
					cmd = [cmd].concat(args).map(StringTools.quoteUnixArg).join(" ");
					return NativeSys.sys_command(cmd);
			}
		}
	}

	public static function exit( code : Int ) : Void {
		untyped __global__.__hxcpp_exit(code);
	}

	public static function time() : Float {
		return NativeSys.sys_time();
	}

	public static function cpuTime() : Float {
		return NativeSys.sys_cpu_time();
	}

	@:deprecated("Use programPath instead") public static function executablePath() : String {
		return NativeSys.sys_exe_path();
	}

	public static function programPath() : String {
		return NativeSys.sys_exe_path();
	}

	public static function environment() : Map<String,String> {
		var vars:Array<String> = NativeSys.sys_env();
		var result = new haxe.ds.StringMap<String>();
		var i = 0;
		while(i<vars.length) {
			result.set( vars[i], vars[i+1] );
			i+=2;
		}
		return result;
	}

}
