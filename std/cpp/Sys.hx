package cpp;

class Sys {

	public static function args() : Array<String> untyped {
		return __global__.__get_args();
	}

	public static function getEnv( s : String ):String {
		var v = get_env(s);
		if( v == null )
			return null;
		return v;
	}

	public static function putEnv( s : String, v : String ) {
		put_env(s,v);
	}

	public static function sleep( seconds : Float ) {
		_sleep(seconds);
	}

	public static function setTimeLocale( loc : String ) : Bool {
		return set_time_locale(loc);
	}

	public static function getCwd() : String {
		return new String(get_cwd());
	}

	public static function setCwd( s : String ) {
		set_cwd(s);
	}

	public static function systemName() : String {
		return sys_string();
	}

	public static function escapeArgument( arg : String ) : String {
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
		return sys_command(cmd);
	}

	public static function exit( code : Int ) {
		sys_exit(code);
	}

	public static function time() : Float {
		return sys_time();
	}

	public static function cpuTime() : Float {
		return sys_cpu_time();
	}

	public static function executablePath() : String {
		return new String(sys_exe_path());
	}

	public static function environment() : Hash<String> {
		var vars:Array<String> = sys_env();
		var result = new Hash<String>();
		var i = 0;
		while(i<vars.length) {
			result.set( vars[i], vars[i+1] );
			i+=2;
		}
		return result;
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
	private static var sys_cpu_time = Lib.load("std","sys_cpu_time",0);
	private static var sys_exe_path = Lib.load("std","sys_exe_path",0);
	private static var sys_env = Lib.load("std","sys_env",0);

}
