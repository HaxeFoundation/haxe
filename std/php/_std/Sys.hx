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

import php.*;
import sys.io.FileOutput;
import sys.io.FileInput;

@:coreApi class Sys {

	public static inline function print( v : Dynamic ) : Void {
		Global.echo(Std.string(v));
	}

	public static function println( v : Dynamic ) : Void {
		print(v);
		print("\n");
	}

	public static function args() : Array<String> {
		if (Global.array_key_exists('argv', SuperGlobal._SERVER)) {
			return @:privateAccess Array.wrap(Global.array_slice(SuperGlobal._SERVER['argv'], 1));
		} else {
			return [];
		}
	}

	public static function getEnv( s : String ) : String {
		var value = Global.getenv(s);
		return value == false ? null : value;
	}

	public static inline function putEnv( s : String, v : String ) : Void {
		Global.putenv('$s=$v');
	}

	public static inline function sleep( seconds : Float ) : Void {
		return Global.usleep(Std.int(seconds * 1000000));
	}

	public static inline function setTimeLocale( loc : String ) : Bool {
		return Global.setlocale(Const.LC_TIME, loc) != false;
	}

	public static function getCwd() : String {
		var cwd = Global.getcwd();
		if (cwd == false) return null;
		var l = (cwd:String).substr(-1);
		return (cwd:String) + (l == '/' || l == '\\' ? '' : '/');
	}

	public static inline function setCwd( s : String ) : Void {
		Global.chdir(s);
	}

	public static function systemName() : String {
		var s = Global.php_uname('s');
		var p = s.indexOf(" ");
		return (p >= 0 ? s.substr(0, p) : s);
	}

	public static function command( cmd : String, ?args : Array<String> ) : Int {
		if (args != null) {
			switch (systemName()) {
				case "Windows":
					cmd = [
						for (a in [StringTools.replace(cmd, "/", "\\")].concat(args))
						StringTools.quoteWinArg(a, true)
					].join(" ");
				case _:
					cmd = [cmd].concat(args).map(StringTools.quoteUnixArg).join(" ");
			}
		}
		var result = Boot.deref(0);
		Global.system(cmd, result);
		return result;
	}

	public static inline function exit( code : Int ) : Void {
		Global.exit(code);
	}

	public static inline function time() : Float {
		return Global.microtime(true);
	}

	public static function cpuTime() : Float {
		return time() - SuperGlobal._SERVER['REQUEST_TIME'];
	}

	@:deprecated("Use programPath instead") public static inline function executablePath() : String {
		return SuperGlobal._SERVER['SCRIPT_FILENAME'];
	}

	// It has to be initialized before any call to Sys.setCwd()...
	static var _programPath = sys.FileSystem.fullPath(SuperGlobal._SERVER['SCRIPT_FILENAME']);
	public static function programPath() : String {
		return _programPath;
	}

	public static function environment() : Map<String,String> {
		return php.Lib.hashOfAssociativeArray(SuperGlobal._SERVER);
	}

	public static function stdin() : haxe.io.Input {
		var p = Global.defined('STDIN') ? Const.STDIN : Global.fopen('php://stdin', 'r');
		return @:privateAccess new FileInput(p);
	}

	public static function stdout() : haxe.io.Output {
		var p = Global.defined('STDOUT') ? Const.STDOUT : Global.fopen('php://stdout', 'w');
		return @:privateAccess new FileOutput(p);
	}

	public static function stderr() : haxe.io.Output {
		var p = Global.defined('STDERR') ? Const.STDERR : Global.fopen('php://stderr', 'w');
		return @:privateAccess new FileOutput(p);
	}

	public static function getChar( echo : Bool ) : Int {
		var c = Global.fgetc(Const.STDIN);
		if (c == false) {
			return 0;
		} else {
			if(echo) Global.echo(c);
			return Global.ord(c);
		}
	}

}
