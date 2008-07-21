package php;

class Lib {
	/**
		Print the specified value on the default output.
	**/
	public static function print( v : Dynamic ) : Void {
		untyped __call__("echo", Std.string(v));
	}

	/*
		Print the specified value on the default output followed by a newline character.
	*/
	public static function println( v : Dynamic ) : Void {
		print(v);
		print("\n");
	}
	
	public static function dump(v : Dynamic) : Void {
		untyped __call__("var_dump", v);
	}
	
	/**
		Serialize using native PHP serialization. This will return a Binary string that can be
		stored for long term usage.
	**/
	public static function serialize( v : Dynamic ) : String {
		return untyped __call__("serialize", v);
	}

	/**
		Unserialize a string using native PHP serialization. See [serialize].
	**/
	public static function unserialize( s : String ) : Dynamic {
		return untyped __call__("unserialize", s);
	}
	
	public static function extensionLoaded(name : String) {
		return untyped __call__("extension_loaded", name);
	}
	
	public static function isCli() : Bool {
		return untyped __php__("0 == strncasecmp(PHP_SAPI, 'cli', 3)");
	}
	
	public static function exit(?msg : String) {
		return untyped __call__("exit", msg);
	}
	
	public static function exitCode(code : Int) {
		return untyped __call__("exit", code);
	}
	
	public static function printFile(file : String) {
		var h = untyped __call__("fopen", file,  "r");
		return untyped __call__("fpassthru", h);
	}
}


