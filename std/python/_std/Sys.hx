import python.lib.Time;


@:coreApi
class Sys {

	static var environ:haxe.ds.StringMap<String>;

	public static function time():Float {
		return Time.time()/1000;
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
		environ.set(s, v);
	}

	public static function environment() : haxe.ds.StringMap<String> {
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
		return "";
	}

	public static function command( cmd : String, ?args : Array<String> ) : Int {
		var args = args == null ? [cmd] : [cmd].concat(args);
		return python.lib.Subprocess.call(args);
	}

	public static function cpuTime() : Float {
		return 0.0;
	}

	public static function executablePath() : String {
		return python.lib.Sys.argv[0];
	}

	public static function getChar( echo : Bool ) : Int {
		trace(python.lib.Sys.platform);
		var ch = switch (python.lib.Sys.platform) {
			case x if (StringTools.startsWith(x, "linux") || x =="darwin"):
				var fd = python.lib.Sys.stdin.fileno();
				var old = python.lib.Termios.tcgetattr(fd);

				var restore = python.lib.Termios.tcsetattr.bind(fd, python.lib.Termios.TCSADRAIN, old);

				try {
					python.lib.Tty.setraw(fd);
					var ch = python.lib.Sys.stdin.read(1);
					restore();
					ch;
				} catch (e:Dynamic) {
					trace("error" + e);
					restore();
					String.fromCharCode(0);
				}

			case "win32" | "cygwin":
				python.lib.Msvcrt.getch();
			case x :
				throw "platform " + x + " not supported";
		}
		return ch.charCodeAt(0);
	}

	public static function stdin() : haxe.io.Input {
		return null;
	}

	public static function stdout() : haxe.io.Output {
		return null;
	}

	public static function stderr() : haxe.io.Output {
		return null;
	}

	static function __init__():Void {
		environ = new haxe.ds.StringMap();
		var env = python.lib.Os.environ;

		for (key in env.keys()) {
			environ.set(key, env.get(key, null));
		}
	}
}