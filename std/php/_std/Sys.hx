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
		untyped __call__("echo", Std.string(v));
	}

	public static function println( v : Dynamic ) : Void {
		print(v);
		print("\n");
	}

	public static function args() : Array<String> {
		return untyped __call__('array_key_exists', 'argv', __var__('_SERVER')) ? __call__('new _hx_array', __call__('array_slice', __var__('_SERVER', 'argv'), 1)) : [];
	}

	public static function getEnv( s : String ) : String {
		return untyped __call__("getenv", s);
	}

	public static function putEnv( s : String, v : String ) : Void {
		return untyped __call__("putenv", s + "=" + v);
	}

	public static function sleep( seconds : Float ) : Void {
		return untyped __call__("usleep", seconds*1000000);
	}

	public static function setTimeLocale( loc : String ) : Bool {
		return untyped __call__("setlocale", __php__("LC_TIME"), loc) != false;
	}

	public static function getCwd() : String {
		var cwd : String = untyped __call__("getcwd");
		var l = cwd.substr(-1);
		return cwd + (l == '/' || l == '\\' ? '' : '/');
	}

	public static function setCwd( s : String ) : Void {
		untyped __call__("chdir", s);
	}

	public static function systemName() : String {
		var s : String = untyped __call__("php_uname", "s");
		var p : Int;
		if((p = s.indexOf(" ")) >= 0)
			return s.substr(0, p);
		else
			return s;
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
		var result = 0;
		untyped __call__("system", cmd, result);
		return result;
	}

	public static function exit( code : Int ) : Void {
		untyped __call__("exit", code);
	}

	public static function time() : Float {
		return untyped __call__("microtime", true);
	}

	public static function cpuTime() : Float {
		return untyped __call__("microtime", true) - __php__("$_SERVER['REQUEST_TIME']");
	}

	public static function executablePath() : String {
		return untyped __php__("$_SERVER['SCRIPT_FILENAME']");
	}

	public static function environment() : Hash<String> {
		return php.Lib.hashOfAssociativeArray(untyped __php__("$_SERVER"));
	}

	public static function stdin() : haxe.io.Input {
		return untyped new sys.io.FileInput(__call__('fopen', 'php://stdin', "r"));
	}

	public static function stdout() : haxe.io.Output {
		return untyped new sys.io.FileOutput(__call__('fopen', 'php://stdout', "w"));
	}

	public static function stderr() : haxe.io.Output {
		return untyped new sys.io.FileOutput(__call__('fopen', 'php://stderr', "w"));
	}

	public static function getChar( echo : Bool ) : Int {
		var v : Int = untyped __call__("fgetc", __php__("STDIN"));
		if(echo)
			untyped __call__('echo', v);
		return v;
	}

}