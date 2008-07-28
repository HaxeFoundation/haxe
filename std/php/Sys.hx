package php;


class Sys {
	public static function args() : Array<String> {
		return untyped __php__("array_key_exists('argv', $_SERVER) ? $_SERVER['argv'] : array()");
	}

	public static function getEnv( s : String ) : String {
		return untyped __call__("getenv", s);
	}

	public static function putEnv( s : String, v : String ) : Void {
		return untyped __call__("putenv", s + "=" + v);
	}

	public static function sleep( seconds : Float ) {
		return untyped __call__("sleep", seconds);
	}

	public static function setTimeLocale( loc : String ) : Bool {
		return untyped __call__("setlocale", __php__("LC_TIME"), loc) != false;
	}

	public static function getCwd() : String {
		var cwd : String = untyped __call__("getcwd");
		var l = cwd.substr(-1);
		return cwd + (l == '/' || l == '\\' ? '' : '/');
	}

	public static function setCwd( s : String ) {
		return untyped __call__("chdir", s);
	}

	public static function systemName() : String {
		var s : String = untyped __call__("php_uname", "s");
		var p : Int;
		if((p = s.indexOf(" ")) >= 0)
			return s.substr(0, p);
		else
			return s;
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
		return untyped __call__("system", cmd);
	}

	public static function exit( code : Int ) {
		return untyped __call__("exit", code);
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
		return Lib.hashOfAssociativeArray(untyped __php__("$_SERVER"));
	}
}
