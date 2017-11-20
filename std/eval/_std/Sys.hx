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

@:require(sys)
@:coreApi
class Sys {
	@:extern static public function print(v:Dynamic):Void { }
	@:extern static public function println(v:Dynamic):Void { }
	@:extern static public function args():Array<String> { return []; }
	@:extern static public function getEnv(s:String):String { return ""; }
	@:extern static public function putEnv(s:String, v:String):Void { }
	@:extern static public function environment():Map<String, String> { return null; }
	@:extern static public function sleep(seconds:Float):Void { }
	@:extern static public function setTimeLocale(loc:String):Bool { return false; }
	@:extern static public function getCwd():String { return ""; }
	@:extern static public function setCwd(s:String):Void { }
	@:extern static public function systemName():String { return ""; }

	@:extern static function _command(cmd:String):Int { return 0; }

	static public function command(cmd:String, ?args:Array<String>):Int {
		if (args == null) {
			return _command(cmd);
		} else {
			switch (systemName()) {
				case "Windows":
					cmd = [
						for (a in [StringTools.replace(cmd, "/", "\\")].concat(args))
						StringTools.quoteWinArg(a, true)
					].join(" ");
					return _command(cmd);
				case _:
					cmd = [cmd].concat(args).map(StringTools.quoteUnixArg).join(" ");
					return _command(cmd);
			}
		}
	}

	static public function executablePath():String { return programPath(); }

	@:extern static public function exit(code:Int):Void { }
	@:extern static public function time():Float { return 0.; }
	@:extern static public function cpuTime():Float { return 0.; }
	@:extern static public function programPath():String { return ""; }
	@:extern static public function getChar(echo:Bool):Int { return 0; }
	@:extern static public function stdin():haxe.io.Input { return (null : sys.io.FileInput); }
	@:extern static public function stdout():haxe.io.Output { return (null : sys.io.FileOutput); }
	@:extern static public function stderr():haxe.io.Output { return (null : sys.io.FileOutput); }
}