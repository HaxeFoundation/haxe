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

using StringTools;

/**
	Various Haxe->PHP compatibility utilities
**/
@:keep
@:dox(hide)
class Boot {
	/**
		Initialization stuff
	**/
	static function __init__() {
		untyped __php__("error_reporting(E_ALL)");
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
	static private var classes = new Map<String,HxClass>();

	@:protected
	var phpClassName : String;

	/**
		Get `HxClass` instance for specified PHP fully qualified class name (E.g. '\some\pack\MyClass')
	**/
	public static function get( phpClassName:String ) : HxClass {
		var cls = classes.get(phpClassName);
		if (cls == null) {
			cls = new HxClass(phpClassName);
			classes.set(phpClassName, cls);
		}

		return cls;
	}

	@:protected
	private function new( phpClassName:String ) : Void {
		this.phpClassName = phpClassName;
	}

	/**
		Returns native PHP fully qualified class name for this type
	**/
	public function getClassName() : String {
		return phpClassName;
	}


	/**
		Implementation for `cast(value, Class<Dynamic>)`
		@throws HException if `value` cannot be casted to this type
	**/
	public function tryCast( value:Dynamic ) : Dynamic {
		inline function isNumber( value ) {
			return untyped __call__("is_int", value) || untyped __call__("is_float", value);
		}

		switch (phpClassName) {
			case '\\Int':
				if (isNumber(value)) {
					return untyped __php__("(int)$value");
				}
			case '\\Float':
				if (isNumber(value)) {
					return untyped __php__("(float)$value");
				}
			case '\\Bool':
				if (untyped __call__("is_bool", value)) {
					return value;
				}
			case '\\String':
				if (untyped __call__("is_string", value)) {
					return value;
				}
			case '\\php7\\NativeArray':
				if (untyped __call__("is_array", value)) {
					return value;
				}
			case _:
				if (untyped __call__("is_object", value) && Std.is(value, cast this)) {
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
		var haxeType = phpClassName;

		var prefix = Boot.getPrefix();
		if (prefix.length > 0 && phpClassName.indexOf(prefix) >= 0) {
			haxeType = haxeType.substr(prefix.length);
		}

		haxeType = phpClassName.replace('\\', '.');
		if (haxeType.charAt(0) == '.') {
			haxeType = haxeType.substr(1);
		}

		return haxeType;
	}
}


/**
	Base class for enum types
**/
// @:keep
// @:dox(hide)
// private class HxEnum {
// 	var constructor : String;
// 	var arguments : Array<Dynamic>;

// 	public function new(constructor:String, arguments:Array<Dynamic>) : Void {
// 		this.constructor = constructor;
// 		this.arguments = arguments;
// 	}
// }