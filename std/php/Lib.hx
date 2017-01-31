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
package php;

/**
	Platform-specific PHP Library. Provides some platform-specific functions 
	for the PHP target, such as conversion from Haxe types to native types 
	and vice-versa.
**/
class Lib {
	/**
		Print the specified value on the default output.
	**/
	public static function print( v : Dynamic ) : Void {
		untyped __call__("echo", Std.string(v));
	}

	/**
		Print the specified value on the default output followed by 
		a newline character.
	**/
	public static function println( v : Dynamic ) : Void {
		print(v);
		print("\n");
	}

	/**
		Displays structured information about one or more expressions 
		that includes its type and value. Arrays and objects are 
		explored recursively with values indented to show structure.
	*/
	public static function dump(v : Dynamic) : Void {
		untyped __call__("var_dump", v);
	}

	/**
		Serialize using native PHP serialization. This will return a binary 
		`String` that can be stored for long term usage.
	**/
	public static function serialize( v : Dynamic ) : String {
		return untyped __call__("serialize", v);
	}

	/**
		Unserialize a `String` using native PHP serialization. See `php.Lib.serialize()`.
	**/
	public static function unserialize( s : String ) : Dynamic {
		return untyped __call__("unserialize", s);
	}

	/**
		Find out whether an extension is loaded.
	*/
	public static function extensionLoaded(name : String) {
		return untyped __call__("extension_loaded", name);
	}

	public static function isCli() : Bool {
		return untyped __php__("(0 == strncasecmp(PHP_SAPI, 'cli', 3))");
	}

	/**
		Output file content from the given file name.
	*/
	public static function printFile(file : String) {
		return untyped __call__("fpassthru", __call__("fopen", file,  "r"));
	}

	public static function toPhpArray(a : Array<Dynamic>) : NativeArray {
		return untyped __field__(a, 'a');
	}

	public static inline function toHaxeArray(a : NativeArray) : Array<Dynamic> {
		return untyped __call__("new _hx_array", a);
	}

	public static function hashOfAssociativeArray<T>(arr : NativeArray) : Map<String,T> {
		var h = new haxe.ds.StringMap<T>();
		untyped h.h = arr;
		return h;
	}

	public static function associativeArrayOfHash(hash : haxe.ds.StringMap<Dynamic>) : NativeArray {
		return untyped hash.h;
	}

	public static function objectOfAssociativeArray(arr : NativeArray) : Dynamic {
		untyped __php__("foreach($arr as $key => $value){
			if(is_array($value)) $arr[$key] = php_Lib::objectOfAssociativeArray($value);
		}");
		return untyped __call__("_hx_anonymous", arr);
	}

	public static function associativeArrayOfObject(ob : Dynamic) : NativeArray {
		return untyped __php__("(array) $ob");
	}

	/**
	 * See the documentation for the equivalent PHP function for details on usage:
	 * <http://php.net/manual/en/function.mail.php>
	 * @param	to
	 * @param	subject
	 * @param	message
	 * @param	?additionalHeaders
	 * @param	?additionalParameters
	 */
	public static function mail(to : String, subject : String, message : String, ?additionalHeaders : String, ?additionalParameters : String) : Bool
	{
		if(null != additionalParameters)
			return untyped __call__("mail", to, subject, message, additionalHeaders, additionalParameters);
		else if(null != additionalHeaders)
			return untyped __call__("mail", to, subject, message, additionalHeaders);
		else
			return untyped __call__("mail", to, subject, message);
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
		var prefix = untyped __prefix__();
		untyped __php__("$_hx_types_array = array();
 		$_hx_cache_content = '';
 		//Calling this function will put all types present in the specified types in the $_hx_types_array
 		_hx_build_paths($pathToLib, $_hx_types_array, array(), $prefix);

 		foreach($_hx_types_array as $val) {
 			//For every type that has been found, create its description
 			$t = null;
 			if($val['type'] == 0) {
 				$t = new _hx_class($val['phpname'], $val['qname'], $val['path']);
 			} else if($val['type'] == 1) {
 				$t = new _hx_enum($val['phpname'], $val['qname'], $val['path']);
 			} else if($val['type'] == 2) {
 				$t = new _hx_interface($val['phpname'], $val['qname'], $val['path']);
 			} else if($val['type'] == 3) {
 				$t = new _hx_class($val['name'], $val['qname'], $val['path']);
 			}
 			//Register the type
 			if(!array_key_exists($t->__qname__, php_Boot::$qtypes)) {
 				_hx_register_type($t);
 			}
 		}
 ");
 	}
}
