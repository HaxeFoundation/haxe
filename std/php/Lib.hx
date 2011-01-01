package php;

class Lib {
	/**
		Print the specified value on the default output.
	**/
	public static function print( v : Dynamic ) : Void {
		untyped __call__("echo", Std.string(v));
	}

	/**
		Print the specified value on the default output followed by a newline character.
	**/
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
		return untyped __php__("(0 == strncasecmp(PHP_SAPI, 'cli', 3))");
	}

	public static function printFile(file : String) {
		return untyped __call__("fpassthru", __call__("fopen", file,  "r"));
	}

	public static function toPhpArray(a : Array<Dynamic>) : NativeArray {
		return untyped __field__(a, '»a');
	}

	public static inline function toHaxeArray(a : NativeArray) : Array<Dynamic> {
		return untyped __call__("new _hx_array", a);
	}

	public static function hashOfAssociativeArray<T>(arr : NativeArray) : Hash<T> {
		var h = new Hash<T>();
		untyped __php__("reset($arr); while(list($k, $v) = each($arr)) $h->set($k, $v)");
		return h;
	}
	
	public static function associativeArrayOfHash(hash : Hash<Dynamic>) : NativeArray {
		return untyped hash.h;
	}

	/**
		For neko compatibility only.
	**/
	public static function rethrow( e : Dynamic ) {
		if(Std.is(e, Exception)) {
			var __rtex__ = e;
			untyped __php__("throw $__rtex__");
		}
		else throw e;
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
	
	/**
	*  Loads types defined in the specified directory.
 	*/
 	public static function loadLib(pathToLib : String) : Void
 	{
		untyped __php__("$_hx_types_array = array();
 		$_hx_cache_content = '';
 		//Calling this function will put all types present in the specified types in the $_hx_types_array
 		_hx_build_paths($pathToLib, $_hx_types_array, array());
 
 		for($i=0;$i<count($_hx_types_array);$i++) {
 			//For every type that has been found, create its description
 			$t = null;
 			if($_hx_types_array[$i]['type'] == 0) {
 				$t = new _hx_class($_hx_types_array[$i]['phpname'], $_hx_types_array[$i]['qname'], $_hx_types_array[$i]['path']);
 			} else if($_hx_types_array[$i]['type'] == 1) {
 				$t = new _hx_enum($_hx_types_array[$i]['phpname'], $_hx_types_array[$i]['qname'], $_hx_types_array[$i]['path']);
 			} else if($_hx_types_array[$i]['type'] == 2) {
 				$t = new _hx_interface($_hx_types_array[$i]['phpname'], $_hx_types_array[$i]['qname'], $_hx_types_array[$i]['path']);
 			} else if($_hx_types_array[$i]['type'] == 3) {
 				$t = new _hx_class($_hx_types_array[$i]['name'], $_hx_types_array[$i]['qname'], $_hx_types_array[$i]['path']);
 			}
 			//Register the type
 			if(!array_key_exists($t->__qname__, php_Boot::$qtypes)) {
 				_hx_register_type($t);
 			}
 		}
 ");
 	}
}