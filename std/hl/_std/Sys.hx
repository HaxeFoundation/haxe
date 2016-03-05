class SysError {
	public var msg : String;
	public function new(msg) {
		this.msg = msg;
	}
	@:keep public function toString() {
		return "SysError("+msg+")";
	}
}

@:coreApi
@:access(String)
class Sys {

	public static function print( v : Dynamic ) : Void {
		_print(Std.string(v).bytes);
	}

	public static function println( v : Dynamic ) : Void {
		_print(Std.string(v).bytes);
		_print("\n".bytes);
	}

	public static function args() : Array<String> {
		return [for( a in sys_args() ) String.fromUCS2(a)];
	}

	public static function stdin() : haxe.io.Input {
		return @:privateAccess new sys.io.FileInput(sys_stdin());
	}

	public static function stdout() : haxe.io.Output {
		return @:privateAccess new sys.io.FileOutput(sys_stdout());
	}

	public static function stderr() : haxe.io.Output {
		return @:privateAccess new sys.io.FileOutput(sys_stderr());
	}

	public static function getEnv( s : String ) : String {
		var v = get_env(s.bytes);
		if( v == null )
			return null;
		return String.fromUCS2(v);
	}

	public static function putEnv( s : String, v : String ) : Void {
		put_env(s.bytes,if( v == null ) null else v.bytes);
	}

	@:hlNative("std","sys_sleep")
	public static function sleep( seconds : Float ) : Void {
	}

	public static function setTimeLocale( loc : String ) : Bool {
		return set_time_locale(loc.bytes);
	}

	public static function getCwd() : String {
		return String.fromUCS2(get_cwd());
	}

	public static function setCwd( s : String ) : Void {
		if( !set_cwd(s.bytes) ) throw new SysError("Failed to set path to " + s);
	}

	public static function systemName() : String {
		return String.fromUCS2(sys_string());
	}

	public static function command( cmd : String, ?args : Array<String> ) : Int {
		var code = 0;
		var ok;
		if (args == null) {
			ok = sys_command(cmd.bytes, code);
		} else {
			switch (systemName()) {
				case "Windows":
					cmd = [
						for (a in [StringTools.replace(cmd, "/", "\\")].concat(args))
						StringTools.quoteWinArg(a, true)
					].join(" ");
					ok = sys_command(cmd.bytes, code);
				case _:
					cmd = [cmd].concat(args).map(StringTools.quoteUnixArg).join(" ");
					ok = sys_command(cmd.bytes, code);
			}
		}
		if( !ok ) throw new SysError("Failed to run command " + cmd);
		return code;
	}

	public static function executablePath() : String {
		return String.fromUCS2(sys_exe_path());
	}

	public static function environment() : Map<String,String> {
		var env = sys_env();
		var h = new haxe.ds.StringMap();
		for( i in 0...env.length >> 1 ) {
			var p = i << 1;
			h.set(String.fromUCS2(env[p]), String.fromUCS2(env[p + 1]));
		}
		return h;
	}

	@:hlNative("std","sys_time") public static function time() : Float { return 0.; };
	@:hlNative("std","sys_exit") public static function exit( code : Int ) : Void {};
	@:hlNative("std", "sys_cpu_time") public static function cpuTime() : Float { return 0.; };
	@:hlNative("std", "sys_get_char") public static function getChar( echo : Bool ) : Int { return 0; }

	@:hlNative("std","sys_print") static function _print( v : hl.types.Bytes ) : Void {};
	@:hlNative("std", "sys_stdin") static function sys_stdin() : sys.io.File.FileHandle { return null; }
	@:hlNative("std", "sys_stdout") static function sys_stdout() : sys.io.File.FileHandle { return null; }
	@:hlNative("std", "sys_stderr") static function sys_stderr() : sys.io.File.FileHandle { return null; }
	@:hlNative("std", "sys_args") static function sys_args() : hl.types.NativeArray<hl.types.Bytes> { return null; }
	@:hlNative("std", "sys_get_env") static function get_env( key : hl.types.Bytes ) : hl.types.Bytes { return null; }
	@:hlNative("std", "sys_put_env") static function put_env( key : hl.types.Bytes, val : hl.types.Bytes ) : Void {}
	@:hlNative("std", "sys_env") static function sys_env() : hl.types.NativeArray<hl.types.Bytes> { return null; }
	@:hlNative("std", "sys_set_time_locale") static function set_time_locale( loc : hl.types.Bytes ) : Bool { return true; }
	@:hlNative("std", "sys_get_cwd") static function get_cwd() : hl.types.Bytes { return null; }
	@:hlNative("std", "sys_set_cwd") static function set_cwd( path : hl.types.Bytes ) : Bool { return true; }
	@:hlNative("std", "sys_command") static function sys_command( cmd : hl.types.Bytes, code : hl.types.Ref<Int> ) : Bool { return false; }
	@:hlNative("std", "sys_exe_path") static function sys_exe_path() : hl.types.Bytes { return null; }
	@:hlNative("std", "sys_string") static function sys_string() : hl.types.Bytes { return null; }

}