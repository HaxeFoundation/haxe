/*
 * Copyright (c) 2005-2012, The haXe Project Contributors
 * All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   - Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   - Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE HAXE PROJECT CONTRIBUTORS "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE HAXE PROJECT CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
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

	static function escapeArgument( arg : String ) : String {
		var ok = true;
		for( i in 0...arg.length )
			switch( arg.charCodeAt(i) ) {
			case 32, 34: // [space] "
				ok = false;
			case 0, 13, 10: // [eof] [cr] [lf]
				arg = arg.substr(0,i);
			}
		if( ok )
			return arg;
		return '"'+arg.split('"').join('\\"')+'"';
	}

	public static function command( cmd : String, ?args : Array<String> ) : Int {
		if( args != null ) {
			cmd = escapeArgument(cmd);
			for( a in args )
				cmd += " "+escapeArgument(a);
		}
		return sys_command(untyped cmd.__s);
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

	public static function executablePath() : String {
		return new String(sys_exe_path());
	}

	public static function environment() : Hash<String> {
		var l : Array<Dynamic> = sys_env();
		var h = new Hash();
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
	private static var sys_env = neko.Lib.load("std","sys_env",0);

	private static var file_stdin = neko.Lib.load("std","file_stdin",0);
	private static var file_stdout = neko.Lib.load("std","file_stdout",0);
	private static var file_stderr = neko.Lib.load("std","file_stderr",0);
	private static var getch = neko.Lib.load("std","sys_getch",1);

}
