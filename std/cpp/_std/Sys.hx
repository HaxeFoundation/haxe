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
		return sys_command(cmd);
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
		var vars:Array<String> = sys_env();
		var result = new Hash<String>();
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
	private static var sys_exit = cpp.Lib.load("std","sys_exit",1);
	private static var sys_time = cpp.Lib.load("std","sys_time",0);
	private static var sys_cpu_time = cpp.Lib.load("std","sys_cpu_time",0);
	private static var sys_exe_path = cpp.Lib.load("std","sys_exe_path",0);
	private static var sys_env = cpp.Lib.load("std","sys_env",0);

	private static var file_stdin = cpp.Lib.load("std","file_stdin",0);
	private static var file_stdout = cpp.Lib.load("std","file_stdout",0);
	private static var file_stderr = cpp.Lib.load("std","file_stderr",0);

	private static var getch = cpp.Lib.load("std","sys_getch",1);
}