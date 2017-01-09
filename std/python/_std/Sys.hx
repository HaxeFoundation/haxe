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
import python.lib.Time;
import python.lib.Os;
import sys.io.FileInput;
import sys.io.FileOutput;

@:coreApi
class Sys {

	static var environ:haxe.ds.StringMap<String> = {
		environ = new haxe.ds.StringMap();

		var env = Os.environ;

		for (key in env.keys()) {
			environ.set(key, env.get(key, null));
		}
		environ;
	}

	public static inline function time():Float {
		return Time.time();
	}

	public static function exit(code:Int):Void {
		python.lib.Sys.exit(code);
	}

	public static function print(v:Dynamic):Void {
		python.Lib.print(v);
	}

	public static function println(v:Dynamic):Void {
		python.Lib.println(v);
	}

	public static function args() : Array<String> {
		var argv = python.lib.Sys.argv;
		return argv.slice(1);
	}

	public static function getEnv( s : String ) : String {
		return environ.get(s);
	}

	public static function putEnv( s : String, v : String ) : Void {
		python.lib.Os.putenv(s, v);
		environ.set(s, v);
	}

	public static function environment() : Map<String,String> {
		return environ;
	}

	public static function sleep( seconds : Float ) : Void {
		python.lib.Time.sleep(seconds);
	}

	public static function setTimeLocale( loc : String ) : Bool {
		return false;
	}

	public static function getCwd() : String {
		return python.lib.Os.getcwd();
	}

	public static function setCwd( s : String ) : Void {
		python.lib.Os.chdir(s);
	}

	public static function systemName() : String {
		return switch (python.lib.Sys.platform) {
			case x if (StringTools.startsWith(x, "linux")):
				"Linux";
			case "darwin": "Mac";
			case "win32" | "cygwin" : "Windows";
			case _ :
				throw "not supported platform";
		}
	}

	public static function command( cmd : String, ?args : Array<String> ) : Int {
		return
			if (args == null)
				python.lib.Subprocess.call(cmd, { shell: true });
			else
				python.lib.Subprocess.call([cmd].concat(args));
	}

	public static inline function cpuTime() : Float {
		return python.lib.Timeit.default_timer();
	}

	@:deprecated("Use programPath instead") public static function executablePath() : String {
		return python.lib.Sys.argv[0];
	}

	// It has to be initialized before any call to Sys.setCwd()...
	static var _programPath = sys.FileSystem.fullPath(python.lib.Inspect.getsourcefile(Sys));
	public static function programPath() : String {
		return _programPath;
	}

	public static function getChar( echo : Bool ) : Int {

		var ch = switch (systemName()) {
			case "Linux" | "Mac":
				var fd = python.lib.Sys.stdin.fileno();
				var old = python.lib.Termios.tcgetattr(fd);

				var restore = python.lib.Termios.tcsetattr.bind(fd, python.lib.Termios.TCSADRAIN, old);

				try {
					python.lib.Tty.setraw(fd);
					var x = python.lib.Sys.stdin.read(1);
					restore();
					x.charCodeAt(0);
				} catch (e:Dynamic) {
					restore();
					throw e;
				}

			case "Windows":
				python.lib.Msvcrt.getch().decode("utf-8").charCodeAt(0);
			case x :
				throw "platform " + x + " not supported";
		}
		if (echo) {
			python.Lib.print(String.fromCharCode(ch));
		}
		return ch;
	}

	public static function stdin() : haxe.io.Input {
		return python.io.IoTools.createFileInputFromText(python.lib.Sys.stdin);
	}

	public static function stdout() : haxe.io.Output {
		return python.io.IoTools.createFileOutputFromText(python.lib.Sys.stdout);
	}

	public static function stderr() : haxe.io.Output {
		return python.io.IoTools.createFileOutputFromText(python.lib.Sys.stderr);
	}


}