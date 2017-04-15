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
		var ret:Dynamic = untyped __call__("getenv", s);
		return ret == false ? null : ret;
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

	@:deprecated("Use programPath instead") public static function executablePath() : String {
		return untyped __php__("$_SERVER['SCRIPT_FILENAME']");
	}

	// It has to be initialized before any call to Sys.setCwd()...
	static var _programPath = sys.FileSystem.fullPath(untyped __php__("$_SERVER['SCRIPT_FILENAME']"));
	public static function programPath() : String {
		return _programPath;
	}

	public static function environment() : Map<String,String> {
		return php.Lib.hashOfAssociativeArray(untyped __php__("$_SERVER"));
	}

	public static function stdin() : haxe.io.Input {
		var p = untyped __php__("defined('STDIN') ? STDIN : fopen('php://stdin', 'r')");
		return untyped new sys.io.FileInput(p);
	}

	public static function stdout() : haxe.io.Output {
		var p = untyped __php__("defined('STDOUT') ? STDOUT : fopen('php://stdout', 'w')");
		return untyped new sys.io.FileOutput(p);
	}

	public static function stderr() : haxe.io.Output {
		var p = untyped __php__("defined('STDERR') ? STDERR : fopen('php://stderr', 'w')");
		return untyped new sys.io.FileOutput(p);
	}

	public static function getChar( echo : Bool ) : Int {
		var v : Int = untyped __call__("fgetc", __php__("STDIN"));
		if(echo)
			untyped __call__('echo', v);
		return v;
	}

}
