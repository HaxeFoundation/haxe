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

	public static function time():Float {
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
		var args = args == null ? [cmd] : [cmd].concat(args);
		return python.lib.Subprocess.call(args);
	}

	public static function cpuTime() : Float {
		return python.lib.Time.clock();
	}

	public static function executablePath() : String {
		return python.lib.Sys.argv[0];
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