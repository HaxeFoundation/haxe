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

	public static function hashOfAssociativeArray<T>(arr : Dynamic) : Hash<T> {
		var h = new Hash<T>();
		untyped __php__("foreach($arr as $k => $v) $h->set($k, $v)");
		return h;
	}

	/**
		For neko compatibility only.
	**/
	public static function rethrow( e : Dynamic ) {
		throw e;
	}

	static function appendType(o : Dynamic, path : Array<String>, t : Dynamic) {
		var name = path.shift();
		if(path.length == 0)
			untyped __php__("$o->$name = $t");
		else {
			var so = untyped __call__("isset", __php__("$o->$name")) ? __php__("$o->$name") : {};
			appendType(so, path, t);
			untyped __php__("$o->$name = $so");
		}
	}

	public static function getClasses() {
		var path : String = null;
		var o = {};
		untyped __call__('reset', php.Boot.qtypes);
		while((path = untyped __call__('key', php.Boot.qtypes)) != null) {
			appendType(o, path.split('.'), untyped php.Boot.qtypes[path]);
			untyped __call__('next',php.Boot.qtypes);
		}
		return o;
	}
}