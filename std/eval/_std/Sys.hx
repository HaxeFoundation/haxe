/*
 * Copyright (C)2005-2019 Haxe Foundation
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

import haxe.SysTools;

@:require(sys)
@:coreApi
class Sys {
	extern static public function print(v:Dynamic):Void;

	extern static public function println(v:Dynamic):Void;

	extern static public function args():Array<String>;

	extern static public function getEnv(s:String):String;

	extern static public function putEnv(s:String, v:Null<String>):Void;

	extern static public function environment():Map<String, String>;

	extern static public function sleep(seconds:Float):Void;

	extern static public function setTimeLocale(loc:String):Bool;

	extern static public function getCwd():String;

	extern static public function setCwd(s:String):Void;

	extern static public function systemName():String;

	extern static function _command(cmd:String):Int;

	static public function command(cmd:String, ?args:Array<String>):Int {
		if (args == null) {
			return _command(cmd);
		} else {
			switch (systemName()) {
				case "Windows":
					cmd = [
						for (a in [StringTools.replace(cmd, "/", "\\")].concat(args))
							SysTools.quoteWinArg(a, true)
					].join(" ");
					return _command(cmd);
				case _:
					cmd = [cmd].concat(args).map(SysTools.quoteUnixArg).join(" ");
					return _command(cmd);
			}
		}
	}

	static public function executablePath():String {
		return programPath();
	}

	extern static public function exit(code:Int):Void;

	extern static public function time():Float;

	extern static public function cpuTime():Float;

	extern static public function programPath():String;

	extern static public function getChar(echo:Bool):Int;

	extern static public function stdin():haxe.io.Input;

	extern static public function stdout():haxe.io.Output;

	extern static public function stderr():haxe.io.Output;

	static function __init__():Void {
		// This nonsense causes the classes to be loaded. Otherwise they might not make
		// it into the interpreter, and then stderr() et. al. don't work.
		var _ = (null : sys.io.FileOutput);
		var _ = (null : sys.io.FileInput);
	}
}
