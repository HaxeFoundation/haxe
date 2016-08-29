/*
 * Copyright (C)2005-2016 Haxe Foundation
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
package php7;

import haxe.PosInfos;

using StringTools;
using php7.Global;

/**
	Various Haxe->PHP compatibility utilities
**/
@:keep
@:dox(hide)
class Boot {
	/**
		Initialization stuff.
		This method is called once before invoking any Haxe-generated user code.
	**/
	static function __init__() {
		if (!Global.defined('HAXE_CUSTOM_ERROR_HANDLER') || !Const.HAXE_CUSTOM_ERROR_HANDLER) {
			var previousLevel = Global.error_reporting(Const.E_ALL);
			var previousHandler = Global.set_error_handler(
				function (errno:Int, errstr:String, errfile:String, errline:Int) {
					if (Global.error_reporting() & errno == 0) {
						return false;
					}
					throw new ErrorException(errstr, 0, errno, errfile, errline);
				}
			);
			//Already had user-defined handler. Return it.
			if (previousHandler != null) {
				Global.error_reporting(previousLevel);
				Global.set_error_handler(previousHandler);
			}
		}
	}

	/**
		Returns root namespace based on a value of `--php-prefix` compiler flag.
		Returns empty string if no `--php-prefix` provided.
	**/
	public static function getPrefix() : String {
		return untyped __php__("self::PHP_PREFIX");
	}

	/**
		Get Class<T> instance for PHP fully qualified class name (E.g. '\some\pack\MyClass')
		It's always the same instance for the same `phpClassName`
	**/
	public static function getClass( phpClassName:String ) : HxClass {
		return HxClass.get(phpClassName);
	}

	/**
		Associate PHP class name with Haxe class name
	**/
	public static function registerClass( phpClassName:String, haxeClassName:String ) : Void {
		HxClass.register(phpClassName, haxeClassName);
	}

	/**
		`trace()` implementation
	**/
	public static function trace( value:Dynamic, infos:PosInfos ) : Void {
		if (infos != null) {
			Global.echo('${infos.fileName}:${infos.lineNumber}: ');
		}
		Global.echo(stringify(value) + '\n');
	}

	/**
		Returns string representation of `value`
	**/
	public static function stringify( value : Dynamic ) : String {
		if (value == null) {
			return 'null';
		}
		if (value.is_string()) {
			return value;
		}
		if (value.is_int() || value.is_float()) {
			return '' + value;
		}
		if (value.is_bool()) {
			return value ? 'true' : 'false';
		}
		if (value.is_array()) {
			var strings = Global.array_map(function (item) return stringify(item), value);
			return '[' + Global.implode(',', strings) + ']';
		}
		if (value.is_object()) {
			if (value.method_exists('__toString')) {
				return value.__toString();
			}
			if (value.method_exists('toString')) {
				return value.toString();
			}
			if (untyped __php__("$value instanceof \\StdClass")) {
				if (value.toString.isset() && value.toString.is_callable()) {
					return value.toString();
				}
				var result = new NativeIndexedArray<String>();
				var data = Global.get_object_vars(value);
				for (key in data.array_keys()) {
					result.array_push('$key:' + stringify(data[key]));
				}
				return '{' + Global.implode(',', result) + '}';
			}
			var hxClass = getClass(Global.get_class(value));
			return '[object ' + hxClass.toString() + ']';
		}
		throw "Unable to stringify value";
	}

	/**
		If `value` is `null` returns `"null"`. Otherwise returns `value`.
	**/
	public static function stringOrNull( value:Null<String> ) : String {
		return (value == null ? 'null' : value);
	}

	/**
		`Std.is()` implementation
	**/
	public static function is( value:Dynamic, type:Class<Dynamic> ) : Bool {
		var type : HxClass = cast type;
		var phpType = type.getPhpName();
		switch (phpType) {
			case '\\Dynamic':
				return true;
			case '\\Int':
				return value.is_int();
			case '\\Float':
				return value.is_float();
			case '\\Bool':
				return value.is_bool();
			case '\\String':
				return value.is_string();
			case '\\php7\\NativeArray':
				return value.is_array();
			case '\\Enum':
				if (value.is_object()) {
					var hxClass : HxClass = cast HxClass;
					var className = hxClass.getPhpName();
					if (untyped __php__("$value instanceof $className")) {
						var valueType : HxClass = cast value;
						var hxEnum : HxClass = cast HxEnum;
						return Global.is_subclass_of(valueType.getPhpName(), hxEnum.getPhpName());
					}
				}
			case _:
				if (value.is_object()) {
					return untyped __php__("$value instanceof $phpType");
				}
		}
		return false;
	}

	/**
		Performs `left >>> right` operation
	**/
	public static function shiftRightUnsigned( left:Int, right:Int ) : Int {
		if (right == 0) {
			return left;
		} else if (left >= 0) {
			return (left >> right);
		} else {
			return (left >> right) & (0x7fffffff >> (right - 1));
		}
	}

	// /**
	// 	Access fields of dynamic things
	//  */
	// public static function dynamicFieldAccess( target:Dynamic, field:String ) : Dynamic {
	// 	if (field == 'length' && untyped __call__("is_string", target)) {
	// 		return untyped __call__("strlen", $target);
	// 	} else {
	// 		return untyped __php__("$target->$field");
	// 	}
	// }
}


/**
	Class<T> implementation for Haxe->PHP internals
**/
@:keep
@:dox(hide)
private class HxClass {
	@:protected
	static var aliases = new NativeAssocArray<String>();
	@:protected
	static var classes = new NativeAssocArray<HxClass>();

	@:protected
	var phpClassName : String;

	/**
		Associate PHP class name with Haxe class name
	**/
	public static function register( phpClassName:String, haxeClassName:String ) : Void {
		aliases[phpClassName] = haxeClassName;
	}

	/**
		Get `HxClass` instance for specified PHP fully qualified class name (E.g. '\some\pack\MyClass')
	**/
	public static function get( phpClassName:String ) : HxClass {
		if (!Global.isset(classes[phpClassName])) {
			classes[phpClassName] = new HxClass(phpClassName);
		}

		return classes[phpClassName];
	}

	@:protected
	function new( phpClassName:String ) : Void {
		this.phpClassName = phpClassName;
	}

	/**
		Returns native PHP fully qualified class name for this type
	**/
	public function getPhpName() : String {
		return phpClassName;
	}

	/**
		Returns original Haxe fully qualified class name for this type (if exists)
	**/
	public function getHaxeName() : Null<String> {
		return (Global.isset(aliases[phpClassName]) ? aliases[phpClassName] : null);
	}

	/**
		Implementation for `cast(value, Class<Dynamic>)`
		@throws HException if `value` cannot be casted to this type
	**/
	public function tryCast( value:Dynamic ) : Dynamic {
		inline function isNumber( value ) {
			return value.is_int() || value.is_float();
		}

		switch (phpClassName) {
			case '\\Int':
				if (isNumber(value)) {
					return Global.intval(value);
				}
			case '\\Float':
				if (isNumber(value)) {
					return value.floatval();
				}
			case '\\Bool':
				if (value.is_bool()) {
					return value;
				}
			case '\\String':
				if (value.is_string()) {
					return value;
				}
			case '\\php7\\NativeArray':
				if (value.is_array()) {
					return value;
				}
			case _:
				if (value.is_object() && Std.is(value, cast this)) {
					return value;
				}
		}
		throw 'Cannot cast ' + Std.string(value) + ' to ' + toString();
	}

	/**
		Get string representation of this `Class`
	**/
	public function toString() : String {
		return __toString();
	}

	/**
		PHP magic method to get string representation of this `Class`
	**/
	public function __toString() : String {
		return '[class ' + (Global.isset(aliases[phpClassName]) ? aliases[phpClassName] : phpClassName) + ']';
	}
}


/**
	Base class for enum types
**/
@:keep
@:dox(hide)
private class HxEnum {
	static var singletons = new Map<String,HxEnum>();

	var constructor : String;
	var index : Int;
	var args : NativeArray;

	/**
		Returns instances of constructors without arguments
	**/
	public static function singleton( enumClass:String, constructor:String, index:Int ) : HxEnum {
		var key = '$enumClass::$constructor';

		var instance = singletons.get(key);
		if (instance == null) {
			instance = untyped __php__("new $enumClass($constructor)");
			singletons.set(key, instance);
		}

		return instance;
	}

	public function new( constructor:String, index:Int, arguments:NativeArray = null ) : Void {
		this.constructor = constructor;
		this.index = index;
		args = (arguments == null ? untyped __php__("[]") : arguments);
	}

	/**
		Get string representation of this `Class`
	**/
	public function toString() : String {
		return __toString();
	}

	/**
		PHP magic method to get string representation of this `Class`
	**/
	public function __toString() : String {
		var result = constructor;
		if (Global.count(args) > 0) {
			var strings = Global.array_map(function (item) return Boot.stringify(item), args);
			result += '(' + Global.implode(',', strings) + ')';
		}
		return result;
	}
}