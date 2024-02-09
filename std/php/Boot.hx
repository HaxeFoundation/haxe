/*
 * Copyright (C)2005-2019 Haxe Foundation
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

import haxe.PosInfos;
import haxe.extern.EitherType;

using php.Global;

/**
	Various Haxe->PHP compatibility utilities.
	You should not use this class directly.
**/
@:keep
@:dox(hide)
class Boot {
	/** List of Haxe classes registered by their PHP class names  */
	@:protected static var aliases = new NativeAssocArray<String>();

	/** Cache of HxClass instances */
	@:protected static var classes = new NativeAssocArray<HxClass>();

	/** List of getters (for Reflect) */
	@:protected static var getters = new NativeAssocArray<NativeAssocArray<Bool>>();

	/** List of setters (for Reflect) */
	@:protected static var setters = new NativeAssocArray<NativeAssocArray<Bool>>();

	/** Metadata storage */
	@:protected static var meta = new NativeAssocArray<{}>();

	/** Cache for closures created of static methods */
	@:protected static var staticClosures = new NativeAssocArray<NativeAssocArray<HxClosure>>();

	/**
		Initialization stuff.
		This method is called once before invoking any Haxe-generated user code.
	**/
	static function __init__() {
		Global.mb_internal_encoding('UTF-8');
		if (!Global.defined('HAXE_CUSTOM_ERROR_HANDLER') || !Const.HAXE_CUSTOM_ERROR_HANDLER) {
			var previousLevel = Global.error_reporting(Const.E_ALL & ~Const.E_DEPRECATED);
			var previousHandler = Global.set_error_handler(function(errno:Int, errstr:String, errfile:String, errline:Int) {
				if (Global.error_reporting() & errno == 0) {
					return false;
				}
				/*
				 * Division by zero should not throw
				 * @see https://github.com/HaxeFoundation/haxe/issues/7034#issuecomment-394264544
				 */
				if (errno == Const.E_WARNING && errstr == 'Division by zero') {
					return true;
				}
				throw new ErrorException(errstr, 0, errno, errfile, errline);
			});
			// Already had user-defined handler. Return it.
			if (previousHandler != null) {
				Global.error_reporting(previousLevel);
				Global.set_error_handler(previousHandler);
			}
		}
	}

	/**
		Returns root namespace based on a value of `-D php-prefix=value` compiler flag.
		Returns empty string if no `-D php-prefix=value` provided.
	**/
	public static function getPrefix():String {
		return Syntax.code('self::PHP_PREFIX');
	}

	/**
		Register list of getters to be able to call getters using reflection
	**/
	public static function registerGetters(phpClassName:String, list:NativeAssocArray<Bool>):Void {
		getters[phpClassName] = list;
	}

	/**
		Register list of setters to be able to call getters using reflection
	**/
	public static function registerSetters(phpClassName:String, list:NativeAssocArray<Bool>):Void {
		setters[phpClassName] = list;
	}

	/**
		Check if specified property has getter
	**/
	public static function hasGetter(phpClassName:String, property:String):Bool {
		if (!ensureLoaded(phpClassName))
			return false;

		var has = false;
		var phpClassName:haxe.extern.EitherType<Bool, String> = phpClassName;
		do {
			has = Global.isset(getters[phpClassName][property]);
			phpClassName = Global.get_parent_class(phpClassName);
		} while (!has && phpClassName != false && Global.class_exists(phpClassName));

		return has;
	}

	/**
		Check if specified property has setter
	**/
	public static function hasSetter(phpClassName:String, property:String):Bool {
		if (!ensureLoaded(phpClassName))
			return false;

		var has = false;
		var phpClassName:haxe.extern.EitherType<Bool, String> = phpClassName;
		do {
			has = Global.isset(setters[phpClassName][property]);
			phpClassName = Global.get_parent_class(phpClassName);
		} while (!has && phpClassName != false && Global.class_exists(phpClassName));

		return has;
	}

	/**
		Save metadata for specified class
	**/
	public static function registerMeta(phpClassName:String, data:Dynamic):Void {
		meta[phpClassName] = data;
	}

	/**
		Retrieve metadata for specified class
	**/
	public static function getMeta(phpClassName:String):Null<Dynamic> {
		if (!ensureLoaded(phpClassName))
			return null;
		return Global.isset(meta[phpClassName]) ? meta[phpClassName] : null;
	}

	/**
		Associate PHP class name with Haxe class name
	**/
	public static function registerClass(phpClassName:String, haxeClassName:String):Void {
		aliases[phpClassName] = haxeClassName;
	}

	/**
		Returns a list of currently loaded haxe-generated classes.
	**/
	public static function getRegisteredClasses():Array<Class<Dynamic>> {
		var result = [];
		Syntax.foreach(aliases, function(phpName, haxeName) {
			result.push(cast getClass(phpName));
		});
		return result;
	}

	/**
		Returns a list of phpName=>haxeName for currently loaded haxe-generated classes.
	**/
	public static function getRegisteredAliases():NativeAssocArray<String> {
		return aliases;
	}

	/**
		Get Class<T> instance for PHP fully qualified class name (E.g. '\some\pack\MyClass')
		It's always the same instance for the same `phpClassName`
	**/
	public static function getClass(phpClassName:String):HxClass {
		if (phpClassName.charAt(0) == '\\') {
			phpClassName = phpClassName.substr(1);
		}
		if (!Global.isset(classes[phpClassName])) {
			classes[phpClassName] = new HxClass(phpClassName);
		}

		return classes[phpClassName];
	}

	/**
		Returns Class<HxAnon>
	**/
	public static inline function getHxAnon():HxClass {
		return cast HxAnon;
	}

	/**
		Check if provided value is an anonymous object
	**/
	public static inline function isAnon(v:Any):Bool {
		return Std.isOfType(v, HxAnon);
	}

	/**
		Returns Class<HxClass>
	**/
	public static inline function getHxClass():HxClass {
		return cast HxClass;
	}

	/**
		Returns either Haxe class name for specified `phpClassName` or (if no such Haxe class registered) `phpClassName`.
	**/
	public static function getClassName(phpClassName:String):String {
		var hxClass = getClass(phpClassName);
		var name = getHaxeName(hxClass);
		return (name == null ? hxClass.phpClassName : name);
	}

	/**
		Returns original Haxe fully qualified class name for this type (if exists)
	**/
	public static function getHaxeName(hxClass:HxClass):Null<String> {
		switch (hxClass.phpClassName) {
			case 'Int':
				return 'Int';
			case 'String':
				return 'String';
			case 'Bool':
				return 'Bool';
			case 'Float':
				return 'Float';
			case 'Class':
				return 'Class';
			case 'Enum':
				return 'Enum';
			case 'Dynamic':
				return 'Dynamic';
			case _:
		}

		inline function exists()
			return Global.isset(aliases[hxClass.phpClassName]);

		if (exists()) {
			return aliases[hxClass.phpClassName];
		} else if (Global.class_exists(hxClass.phpClassName) && exists()) {
			return aliases[hxClass.phpClassName];
		} else if (Global.interface_exists(hxClass.phpClassName) && exists()) {
			return aliases[hxClass.phpClassName];
		}

		return null;
	}

	/**
		Find corresponding PHP class name.
		Returns `null` if specified class does not exist.
	**/
	public static function getPhpName(haxeName:String):Null<String> {
		var prefix = getPrefix();
		var phpParts = (Global.strlen(prefix) == 0 ? [] : [prefix]);

		var haxeParts = haxeName.split('.');
		for (part in haxeParts) {
			if (isPhpKeyword(part)) {
				part += '_hx';
			}
			phpParts.push(part);
		}

		return phpParts.join('\\');
	}

	/**
		Check if the value of `str` is a reserved keyword in PHP
		@see https://www.php.net/manual/en/reserved.keywords.php
	**/
	@:pure(false)
	static public function isPhpKeyword(str:String):Bool {
		// The body of this method is generated by the compiler
		return false;
	}

	/**
		Unsafe cast to HxClosure
	**/
	public static inline function castClosure(value:Dynamic):HxClosure {
		return value;
	}

	/**
		Unsafe cast to HxClass
	**/
	public static inline function castClass(cls:Class<Dynamic>):HxClass {
		return cast cls;
	}

	/**
		Unsafe cast to HxEnum
	**/
	public static inline function castEnumValue(enm:EnumValue):HxEnum {
		return cast enm;
	}

	/**
		Returns `Class<T>` for `HxClosure`
	**/
	public static inline function closureHxClass():HxClass {
		return cast HxClosure;
	}

	/**
		Implementation for `cast(value, Class<Dynamic>)`
		@throws haxe.ValueError if `value` cannot be casted to this type
	**/
	public static function typedCast(hxClass:HxClass, value:Dynamic):Dynamic {
		if (value == null)
			return null;
		switch (hxClass.phpClassName) {
			case 'Int':
				if (Boot.isNumber(value)) {
					return Global.intval(value);
				}
			case 'Float':
				if (Boot.isNumber(value)) {
					return value.floatval();
				}
			case 'Bool':
				if (value.is_bool()) {
					return value;
				}
			case 'String':
				if (value.is_string()) {
					return value;
				}
			case 'array':
				if (value.is_array()) {
					return value;
				}
			case _:
				if (value.is_object() && Std.isOfType(value, cast hxClass)) {
					return value;
				}
		}
		throw 'Cannot cast ' + Std.string(value) + ' to ' + getClassName(hxClass.phpClassName);
	}

	/**
		Returns string representation of `value`
	**/
	public static function stringify(value:Dynamic, maxRecursion:Int = 10):String {
		if (maxRecursion <= 0) {
			return '<...>';
		}
		if (value == null) {
			return 'null';
		}
		if (value.is_string()) {
			return value;
		}
		if (value.is_int() || value.is_float()) {
			return Syntax.string(value);
		}
		if (value.is_bool()) {
			return value ? 'true' : 'false';
		}
		if (value.is_array()) {
			var strings = Syntax.arrayDecl();
			Syntax.foreach(value, function(key:Dynamic, item:Dynamic) {
				strings.push(Syntax.string(key) + ' => ' + stringify(item, maxRecursion - 1));
			});
			return '[' + Global.implode(', ', strings) + ']';
		}
		if (value.is_object()) {
			if (Std.isOfType(value, Array)) {
				return inline stringifyNativeIndexedArray(value.arr, maxRecursion - 1);
			}
			if (Std.isOfType(value, HxEnum)) {
				var e:HxEnum = value;
				var result = e.tag;
				if (Global.count(e.params) > 0) {
					var strings = Global.array_map(function(item) return Boot.stringify(item, maxRecursion - 1), e.params);
					result += '(' + Global.implode(',', strings) + ')';
				}
				return result;
			}
			if (value.method_exists('toString')) {
				return value.toString();
			}
			if (value.method_exists('__toString')) {
				return value.__toString();
			}
			if (Std.isOfType(value, StdClass)) {
				if (Global.isset(Syntax.field(value, 'toString')) && value.toString.is_callable()) {
					return value.toString();
				}
				var result = new NativeIndexedArray<String>();
				var data = Global.get_object_vars(value);
				for (key in data.array_keys()) {
					result.array_push('$key : ' + stringify(data[key], maxRecursion - 1));
				}
				return '{ ' + Global.implode(', ', result) + ' }';
			}
			if (isFunction(value)) {
				return '<function>';
			}
			if (Std.isOfType(value, HxClass)) {
				return '[class ' + getClassName((value : HxClass).phpClassName) + ']';
			} else {
				return '[object ' + getClassName(Global.get_class(value)) + ']';
			}
		}
		throw "Unable to stringify value";
	}

	static public function stringifyNativeIndexedArray<T>(arr:NativeIndexedArray<T>, maxRecursion:Int = 10):String {
		var strings = Syntax.arrayDecl();
		Syntax.foreach(arr, function(index:Int, value:T) {
			strings[index] = Boot.stringify(value, maxRecursion - 1);
		});
		return '[' + Global.implode(',', strings) + ']';
	}

	static public inline function isNumber(value:Dynamic) {
		return value.is_int() || value.is_float();
	}

	/**
		Check if specified values are equal
	**/
	public static function equal(left:Dynamic, right:Dynamic):Bool {
		if (isNumber(left) && isNumber(right)) {
			return Syntax.equal(left, right);
		}
		if (Std.isOfType(left, HxClosure) && Std.isOfType(right, HxClosure)) {
			return (left : HxClosure).equals(right);
		}
		return Syntax.strictEqual(left, right);
	}

	/**
		Concat `left` and `right` if both are strings or string and null.
		Otherwise return sum of `left` and `right`.
	**/
	public static function addOrConcat(left:Dynamic, right:Dynamic):Dynamic {
		if (left.is_string() || right.is_string()) {
			return (left : String) + (right : String);
		}
		return Syntax.add(left, right);
	}

	@:deprecated('php.Boot.is() is deprecated. Use php.Boot.isOfType() instead')
	public static inline function is(value:Dynamic, type:HxClass):Bool {
		return isOfType(value, type);
	}

	/**
		`Std.isOfType()` implementation
	**/
	public static function isOfType(value:Dynamic, type:HxClass):Bool {
		if (type == null)
			return false;

		var phpType = type.phpClassName;
		#if php_prefix
		var prefix = getPrefix();
		if (Global.substr(phpType, 0, Global.strlen(prefix) + 1) == '$prefix\\') {
			phpType = Global.substr(phpType, Global.strlen(prefix) + 1);
		}
		#end

		switch (phpType) {
			case 'Dynamic':
				return value != null;
			case 'Int':
				return (value.is_int() || (value.is_float() && Syntax.equal(Syntax.int(value), value) && !Global.is_nan(value)))
					&& Global.abs(value) <= 2147483648;
			case 'Float':
				return value.is_float() || value.is_int();
			case 'Bool':
				return value.is_bool();
			case 'String':
				return value.is_string();
			case 'php\\NativeArray', 'php\\_NativeArray\\NativeArray_Impl_':
				return value.is_array();
			case 'Enum' | 'Class':
				if (Std.isOfType(value, HxClass)) {
					var valuePhpClass = (cast value : HxClass).phpClassName;
					var enumPhpClass = (cast HxEnum : HxClass).phpClassName;
					var isEnumType = Global.is_subclass_of(valuePhpClass, enumPhpClass);
					return (phpType == 'Enum' ? isEnumType : !isEnumType);
				}
			case _:
				if (value.is_object()) {
					var type:Class<Dynamic> = cast type;
					return Syntax.instanceof(value, type);
				}
		}
		return false;
	}

	/**
		Check if `value` is a `Class<T>`
	**/
	public static inline function isClass(value:Dynamic):Bool {
		return Std.isOfType(value, HxClass);
	}

	/**
		Check if `value` is an enum constructor instance
	**/
	public static inline function isEnumValue(value:Dynamic):Bool {
		return Std.isOfType(value, HxEnum);
	}

	/**
		Check if `value` is a function
	**/
	public static inline function isFunction(value:Dynamic):Bool {
		return Std.isOfType(value, Closure) || Std.isOfType(value, HxClosure);
	}

	/**
		Check if `value` is an instance of `HxClosure`
	**/
	public static inline function isHxClosure(value:Dynamic):Bool {
		return Std.isOfType(value, HxClosure);
	}

	/**
		Performs `left >>> right` operation
	**/
	public static function shiftRightUnsigned(left:Int, right:Int):Int {
		if (right == 0) {
			return left;
		} else if (left >= 0) {
			return (left >> right) & ~((1 << (8 * Const.PHP_INT_SIZE - 1)) >> (right - 1));
		} else {
			return (left >> right) & (0x7fffffff >> (right - 1));
		}
	}

	/**
		Helper method to avoid "Cannot use temporary expression in write context" error for expressions like this:
		```haxe
		(new MyClass()).fieldName = 'value';
		```
	**/
	static public function deref(value:Dynamic):Dynamic {
		return value;
	}

	/**
		Create Haxe-compatible anonymous structure of `data` associative array
	**/
	static public inline function createAnon(data:NativeArray):Dynamic {
		return new HxAnon(data);
	}

	/**
		Make sure specified class is loaded
	**/
	static public inline function ensureLoaded(phpClassName:String):Bool {
		return Global.class_exists(phpClassName) || Global.interface_exists(phpClassName);
	}

	/**
		Get `field` of a dynamic `value` in a safe manner (avoid exceptions on trying to get a method)
	**/
	static public function dynamicField(value:Dynamic, field:String):Dynamic {
		if (Global.method_exists(value, field)) {
			return closure(value, field);
		}
		if (Global.is_string(value)) {
			value = @:privateAccess new HxDynamicStr(value);
		}
		return Syntax.field(value, field);
	}

	public static function dynamicString(str:String):HxDynamicStr {
		return @:privateAccess new HxDynamicStr(str);
	}

	/**
		Creates Haxe-compatible closure of an instance method.
		@param obj - any object
	**/
	public static function getInstanceClosure(obj:{?__hx_closureCache:NativeAssocArray<HxClosure>}, methodName:String):Null<HxClosure> {
		var result = Syntax.coalesce(obj.__hx_closureCache[methodName], null);
		if (result != null) {
			return result;
		}
		if (!Global.method_exists(obj, methodName) && !Global.isset(Syntax.field(obj, methodName))) {
			return null;
		}
		result = new HxClosure(obj, methodName);
		if (!Global.property_exists(obj, '__hx_closureCache')) {
			obj.__hx_closureCache = new NativeAssocArray();
		}
		obj.__hx_closureCache[methodName] = result;
		return result;
	}

	/**
		Creates Haxe-compatible closure of a static method.
	**/
	public static function getStaticClosure(phpClassName:String, methodName:String) {
		var result = Syntax.coalesce(staticClosures[phpClassName][methodName], null);
		if (result != null) {
			return result;
		}
		result = new HxClosure(phpClassName, methodName);
		if (!Global.array_key_exists(phpClassName, staticClosures)) {
			staticClosures[phpClassName] = new NativeAssocArray();
		}
		staticClosures[phpClassName][methodName] = result;
		return result;
	}

	/**
		Creates Haxe-compatible closure.
		@param type `this` for instance methods; full php class name for static methods
		@param func Method name
	**/
	public static inline function closure(target:Dynamic, func:String):HxClosure {
		return target.is_string() ? getStaticClosure(target, func) : getInstanceClosure(target, func);
	}

	/**
		Get UTF-8 code of the first character in `s` without any checks
	**/
	static public inline function unsafeOrd(s:NativeString):Int {
		var code = Global.ord(s[0]);
		if (code < 0xC0) {
			return code;
		} else if (code < 0xE0) {
			return ((code - 0xC0) << 6) + Global.ord(s[1]) - 0x80;
		} else if (code < 0xF0) {
			return ((code - 0xE0) << 12) + ((Global.ord(s[1]) - 0x80) << 6) + Global.ord(s[2]) - 0x80;
		} else {
			return ((code - 0xF0) << 18) + ((Global.ord(s[1]) - 0x80) << 12) + ((Global.ord(s[2]) - 0x80) << 6) + Global.ord(s[3]) - 0x80;
		}
	}

	static public function divByZero(value:Float):Float {
		return value == 0 ? Const.NAN : (value < 0 ? -Const.INF : Const.INF);
	}
}

/**
	Class<T> implementation for Haxe->PHP internals.
**/
@:keep
@:dox(hide)
private class HxClass {
	public var phpClassName(default, null):String;

	public function new(phpClassName:String):Void {
		this.phpClassName = phpClassName;
	}

	/**
		Magic method to call static methods of this class, when `HxClass` instance is in a `Dynamic` variable.
	**/
	@:phpMagic
	function __call(method:String, args:NativeArray):Dynamic {
		var callback = (phpClassName == 'String' ? Syntax.nativeClassName(HxString) : phpClassName) + '::' + method;
		return Global.call_user_func_array(callback, args);
	}

	/**
		Magic method to get static vars of this class, when `HxClass` instance is in a `Dynamic` variable.
	**/
	@:phpMagic
	function __get(property:String):Dynamic {
		if (Global.defined('$phpClassName::$property')) {
			return Global.constant('$phpClassName::$property');
		} else if (Boot.hasGetter(phpClassName, property)) {
			return Syntax.staticCall(phpClassName, 'get_$property');
		} else if (phpClassName.method_exists(property)) {
			return Boot.getStaticClosure(phpClassName, property);
		} else {
			return Syntax.getStaticField(phpClassName, property);
		}
	}

	/**
		Magic method to set static vars of this class, when `HxClass` instance is in a `Dynamic` variable.
	**/
	@:phpMagic
	function __set(property:String, value:Dynamic):Void {
		if (Boot.hasSetter(phpClassName, property)) {
			Syntax.staticCall(phpClassName, 'set_$property', value);
		} else {
			Syntax.setStaticField(phpClassName, property, value);
		}
	}
}

/**
	Base class for enum types
**/
@:keep
@:dox(hide)
@:allow(php.Boot.stringify)
@:allow(Type)
private class HxEnum {
	final tag:String;
	final index:Int;
	final params:NativeArray;

	public function new(tag:String, index:Int, arguments:NativeArray = null):Void {
		this.tag = tag;
		this.index = index;
		params = (arguments == null ? new NativeArray() : arguments);
	}

	/**
		Get string representation of this `Class`
	**/
	public function toString():String {
		return __toString();
	}

	/**
		PHP magic method to get string representation of this `Class`
	**/
	@:phpMagic
	public function __toString():String {
		return Boot.stringify(this);
	}

	extern public static function __hx__list():Array<String>;
}

/**
	`String` implementation
**/
@:keep
@:dox(hide)
private class HxString {
	public static function toUpperCase(str:String):String {
		return Global.mb_strtoupper(str);
	}

	public static function toLowerCase(str:String):String {
		return Global.mb_strtolower(str);
	}

	public static function charAt(str:String, index:Int):String {
		return index < 0 ? '' : Global.mb_substr(str, index, 1);
	}

	public static function charCodeAt(str:String, index:Int):Null<Int> {
		if (index < 0 || str == '') {
			return null;
		}
		if (index == 0) {
			return Boot.unsafeOrd(str);
		}
		var char = Global.mb_substr(str, index, 1);
		return char == '' ? null : Boot.unsafeOrd(char);
	}

	public static function indexOf(str:String, search:String, startIndex:Int = null):Int {
		if (search.length == 0) {
			return Global.max(0, Global.min(startIndex == null ? 0 : startIndex, str.length));
		}
		if (startIndex == null || startIndex < 0) {
			startIndex = 0;
		} else if (startIndex >= str.length) {
			return -1;
		}
		var index:EitherType<Int, Bool> = if (search == '') {
			var length = str.length;
			startIndex > length ? length : startIndex;
		} else {
			Global.mb_strpos(str, search, startIndex);
		}
		return (index == false ? -1 : index);
	}

	public static function lastIndexOf(str:String, search:String, startIndex:Int = null):Int {
		if (search.length == 0) {
			return Global.max(0, Global.min(startIndex == null ? str.length : startIndex, str.length));
		}
		var start = startIndex;
		if (start == null) {
			start = 0;
		}
		if (startIndex == null) {
			startIndex = 0;
		} else {
			var length = str.length;
			if (start >= 0) {
				start = start - length;
				if (start > 0) {
					start = 0;
				}
			} else if (start < -length) {
				start = -length;
			}
		}
		var index:EitherType<Int, Bool> = if (search == '') {
			var length = str.length;
			startIndex == null
			|| startIndex > length ? length : startIndex;
		} else {
			Global.mb_strrpos(str, search, start);
		}
		if (index == false) {
			return -1;
		} else {
			return index;
		}
	}

	public static function split(str:String, delimiter:String):Array<String> {
		var arr:NativeArray = if (delimiter == '') {
			Global.preg_split('//u', str, -1, Const.PREG_SPLIT_NO_EMPTY);
		} else {
			delimiter = Global.preg_quote(delimiter, '/');
			Global.preg_split('/$delimiter/', str);
		}
		return @:privateAccess Array.wrap(arr);
	}

	public static function substr(str:String, pos:Int, ?len:Int):String {
		return Global.mb_substr(str, pos, len);
	}

	public static function substring(str:String, startIndex:Int, ?endIndex:Int):String {
		if (endIndex == null) {
			if (startIndex < 0) {
				startIndex = 0;
			}
			return Global.mb_substr(str, startIndex);
		}
		if (endIndex < 0) {
			endIndex = 0;
		}
		if (startIndex < 0) {
			startIndex = 0;
		}
		if (startIndex > endIndex) {
			var tmp = endIndex;
			endIndex = startIndex;
			startIndex = tmp;
		}
		return Global.mb_substr(str, startIndex, endIndex - startIndex);
	}

	public static function toString(str:String):String {
		return str;
	}

	public static function fromCharCode(code:Int):String {
		return Global.mb_chr(code);
	}
}

/**
	For Dynamic access which looks like String.
	Instances of this class should not be saved anywhere.
	Instead it should be used to immediately invoke a String field right after instance creation one time only.
**/
@:dox(hide)
@:keep
private class HxDynamicStr extends HxClosure {
	static var hxString:String = (cast HxString : HxClass).phpClassName;

	/**
		Returns HxDynamicStr instance if `value` is a string.
		Otherwise returns `value` as-is.
	**/
	static function wrap(value:Dynamic):Dynamic {
		if (value.is_string()) {
			return new HxDynamicStr(value);
		} else {
			return value;
		}
	}

	static inline function invoke(str:String, method:String, args:NativeArray):Dynamic {
		Global.array_unshift(args, str);
		return Global.call_user_func_array(hxString + '::' + method, args);
	}

	function new(str:String) {
		super(str, null);
	}

	@:phpMagic
	function __get(field:String):Dynamic {
		switch (field) {
			case 'length':
				return (target : String).length;
			case _:
				func = field;
				return this;
		}
	}

	@:phpMagic
	function __call(method:String, args:NativeArray):Dynamic {
		return invoke(target, method, args);
	}

	/**
		@see http://php.net/manual/en/language.oop5.magic.php#object.invoke
	**/
	@:phpMagic
	override public function __invoke() {
		return invoke(target, func, Global.func_get_args());
	}

	/**
		Generates callable value for PHP
	**/
	override public function getCallback(eThis:Dynamic = null):NativeIndexedArray<Dynamic> {
		if (eThis == null) {
			return Syntax.arrayDecl((this : Dynamic), func);
		}
		return Syntax.arrayDecl((new HxDynamicStr(eThis) : Dynamic), func);
	}

	/**
		Invoke this closure with `newThis` instead of `this`
	**/
	override public function callWith(newThis:Dynamic, args:NativeArray):Dynamic {
		if (newThis == null) {
			newThis = target;
		}
		return invoke(newThis, func, args);
	}
}

/**
	Anonymous objects implementation
**/ @:keep @:dox(hide) private class HxAnon extends StdClass {
	public function new(fields:NativeArray = null) {
		super();
		if (fields != null) {
			Syntax.foreach(fields, function(name, value) Syntax.setField(this, name, value));
		}
	}

	@:phpMagic
	function __get(name:String) {
		return null;
	}

	@:phpMagic
	function __call(name:String, args:NativeArray):Dynamic {
		return Syntax.code("($this->{0})(...{1})", name, args);
	}
}

/**
	Closures implementation
**/ @:keep @:dox(hide) private class HxClosure {
	/** `this` for instance methods; php class name for static methods */
	var target:Dynamic;

	/** Method name for methods */
	var func:String;

	/** A callable value, which can be invoked by PHP */
	var callable:Any;

	public function new(target:Dynamic, func:String):Void {
		this.target = target;
		this.func = func;
		// Force runtime error if trying to create a closure of an instance which happen to be `null`
		if (target.is_null()) {
			throw "Unable to create closure on `null`";
		}
		callable = Std.isOfType(target, HxAnon) ? Syntax.field(target, func) : Syntax.arrayDecl(target, func);
	}

	/**
		@see http://php.net/manual/en/language.oop5.magic.php#object.invoke
	**/
	@:phpMagic
	public function __invoke() {
		return Global.call_user_func_array(callable, Global.func_get_args());
	}

	/**
		Generates callable value for PHP
	**/
	public function getCallback(eThis:Dynamic = null):NativeIndexedArray<Dynamic> {
		if (eThis == null) {
			eThis = target;
		}
		if (Std.isOfType(eThis, HxAnon)) {
			return Syntax.field(eThis, func);
		}
		return Syntax.arrayDecl(eThis, func);
	}

	/**
		Check if this is the same closure
	**/
	public function equals(closure:HxClosure):Bool {
		return (target == closure.target && func == closure.func);
	}

	/**
		Invoke this closure with `newThis` instead of `this`
	**/
	public function callWith(newThis:Dynamic, args:NativeArray):Dynamic {
		return Global.call_user_func_array(getCallback(newThis), args);
	}
}
