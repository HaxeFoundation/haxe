/*
 * Copyright (c) 2005, The haXe Project Contributors
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
package neko;

class Sys {

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

	public static function getEnv( s : String ) {
		return new String(get_env(untyped s.__s));
	}

	public static function putEnv( s : String, v : String ) {
		untyped put_env(s.__s,if( v == null ) null else v.__s);
	}

	public static function sleep( seconds : Float ) {
		_sleep(seconds);
	}

	public static function setTimeLocale( loc : String ) : Bool {
		return set_time_locale(untyped loc.__s);
	}

	public static function getCwd() : String {
		return new String(get_cwd());
	}

	public static function setCwd( s : String ) {
		set_cwd(untyped s.__s);
	}

	public static function systemName() : String {
		return new String(sys_string());
	}

	public static function command( cmd : String ) : Int {
		return sys_command(untyped cmd.__s);
	}

	public static function exit( code : Int ) {
		sys_exit(code);
	}

	public static function time() : Float {
		return sys_time();
	}

	public static function executablePath() : String {
		return new String(sys_exe_path());
	}

	public static function environment() : Hash<String> {
		var l = sys_env();
		var h = new Hash();
		while( l != null ) {
			h.set(new String(l[0]),new String(l[1]));
			l = l[2];
		}
		return h;
	}

	public static function stdin() : File {
		return untyped new File(file_stdin());
	}

	public static function stdout() : File {
		return untyped new File(file_stdout());
	}

	public static function stderr() : File {
		return untyped new File(file_stderr());
	}

	private static var get_env = Lib.load("std","get_env",1);
	private static var put_env = Lib.load("std","put_env",2);
	private static var _sleep = Lib.load("std","sys_sleep",1);
	private static var set_time_locale = Lib.load("std","set_time_locale",1);
	private static var get_cwd = Lib.load("std","get_cwd",0);
	private static var set_cwd = Lib.load("std","set_cwd",1);
	private static var sys_string = Lib.load("std","sys_string",0);
	private static var sys_command = Lib.load("std","sys_command",1);
	private static var sys_exit = Lib.load("std","sys_exit",1);
	private static var sys_time = Lib.load("std","sys_time",0);
	private static var sys_exe_path = Lib.load("std","sys_exe_path",0);
	private static var sys_env = Lib.load("std","sys_env",0);
	private static var file_stdin = Lib.load("std","file_stdin",0);
	private static var file_stdout = Lib.load("std","file_stdout",0);
	private static var file_stderr = Lib.load("std","file_stderr",0);

}
