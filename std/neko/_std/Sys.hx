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
@:coreApi class Sys {

	public static function print( v : Dynamic ) : Void {
		untyped __dollar__print(v);
	}

	public static function println( v : Dynamic ) : Void {
		untyped __dollar__print(v,"\n");
	}

	public static function getChar( echo : Bool ) : Int {
		return getch(echo);
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

	public static function args() : Array<String> untyped {
		var a = __dollar__loader.args;
		if( __dollar__typeof(a) != __dollar__tarray )
			return [];
		var r = new Array();
		var i = 0;
		var l = __dollar__asize(a);
		while( i <  l ) {
			if( __dollar__typeof(a[i]) == __dollar__tstring )
				r.push(new String(a[i]));
			i += 1;
		}
		return r;
	}

	public static function getEnv( s : String ) : String {
		var v = get_env(untyped s.__s);
		if( v == null )
			return null;
		return new String(v);
	}

	public static function putEnv( s : String, v : String ) : Void {
		untyped put_env(s.__s,if( v == null ) null else v.__s);
	}

	public static function sleep( seconds : Float ) : Void {
		_sleep(seconds);
	}

	public static function setTimeLocale( loc : String ) : Bool {
		return set_time_locale(untyped loc.__s);
	}

	public static function getCwd() : String {
		return new String(get_cwd());
	}

	public static function setCwd( s : String ) : Void {
		set_cwd(untyped s.__s);
	}

	public static function systemName() : String {
		return new String(sys_string());
	}

	public static function command( cmd : String, ?args : Array<String> ) : Int {
		if (args == null) {
			return sys_command(untyped cmd.__s);
		} else {
			switch (systemName()) {
				case "Windows":
					cmd = [
						for (a in [StringTools.replace(cmd, "/", "\\")].concat(args))
						StringTools.quoteWinArg(a, true)
					].join(" ");
					return sys_command(untyped cmd.__s);
				case _:
					cmd = [cmd].concat(args).map(StringTools.quoteUnixArg).join(" ");
					return sys_command(untyped cmd.__s);
			}
		}
	}

	public static function exit( code : Int ) : Void {
		sys_exit(code);
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
		#if macro
		return null;
		#elseif interp
		return new String(sys_program_path());
		#else
		return sys_program_path;
		#end
	}

	public static function environment() : Map<String,String> {
		var l : Array<Dynamic> = sys_env();
		var h = new haxe.ds.StringMap();
		while( l != null ) {
			h.set(new String(l[0]),new String(l[1]));
			l = l[2];
		}
		return h;
	}

	private static var get_env = neko.Lib.load("std","get_env",1);
	private static var put_env = neko.Lib.load("std","put_env",2);
	private static var _sleep = neko.Lib.load("std","sys_sleep",1);
	private static var set_time_locale = neko.Lib.load("std","set_time_locale",1);
	private static var get_cwd = neko.Lib.load("std","get_cwd",0);
	private static var set_cwd = neko.Lib.load("std","set_cwd",1);
	private static var sys_string = neko.Lib.load("std","sys_string",0);
	private static var sys_command = neko.Lib.load("std","sys_command",1);
	private static var sys_exit = neko.Lib.load("std","sys_exit",1);
	private static var sys_time = neko.Lib.load("std","sys_time",0);
	private static var sys_cpu_time = neko.Lib.load("std","sys_cpu_time",0);
	private static var sys_exe_path = neko.Lib.load("std","sys_exe_path",0);
	#if interp
	private static var sys_program_path = neko.Lib.load("std","sys_program_path",0);
	#elseif !macro
	// It has to be initialized before any call to loadModule or Sys.setCwd()...
	private static var sys_program_path = {
		var m = neko.vm.Module.local().name;
		try {
			sys.FileSystem.fullPath(m);
		} catch (e:Dynamic) {
			// maybe the neko module name was supplied without .n extension...
			if (!StringTools.endsWith(m, ".n")) {
				try {
					sys.FileSystem.fullPath(m + ".n");
				} catch (e:Dynamic) {
					m;
				}
			} else {
				m;
			}
		}
	}
	#end
	private static var sys_env = neko.Lib.load("std","sys_env",0);

	private static var file_stdin = neko.Lib.load("std","file_stdin",0);
	private static var file_stdout = neko.Lib.load("std","file_stdout",0);
	private static var file_stderr = neko.Lib.load("std","file_stderr",0);
	private static var getch = neko.Lib.load("std","sys_getch",1);

}
