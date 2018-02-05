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

import haxe.PosInfos;

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
	public static inline function getPrefix() : String {
		return untyped __php__('self::PHP_PREFIX');
	}

	/**
		Register list of getters to be able to call getters using reflection
	**/
	public static function registerGetters( phpClassName:String, list:NativeAssocArray<Bool> ) : Void {
		getters[phpClassName] = list;
	}

	/**
		Register list of setters to be able to call getters using reflection
	**/
	public static function registerSetters( phpClassName:String, list:NativeAssocArray<Bool> ) : Void {
		setters[phpClassName] = list;
	}

	/**
		Check if specified property has getter
	**/
	public static function hasGetter( phpClassName:String, property:String ) : Bool {
		ensureLoaded(phpClassName);

		var has = false;
		var phpClassName:haxe.extern.EitherType<Bool,String> = phpClassName;
		do {
			has = Global.isset(getters[phpClassName][property]);
			phpClassName = Global.get_parent_class(phpClassName);
		} while (!has && phpClassName != false && Global.class_exists(phpClassName));

		return has;
	}

	/**
		Check if specified property has setter
	**/
	public static function hasSetter( phpClassName:String, property:String ) : Bool {
		ensureLoaded(phpClassName);

		var has = false;
		var phpClassName:haxe.extern.EitherType<Bool,String> = phpClassName;
		do {
			has = Global.isset(setters[phpClassName][property]);
			phpClassName = Global.get_parent_class(phpClassName);
		} while (!has && phpClassName != false && Global.class_exists(phpClassName));

		return has;
	}

	/**
		Save metadata for specified class
	**/
	public static function registerMeta( phpClassName:String, data:Dynamic ) : Void {
		meta[phpClassName] = data;
	}

	/**
		Retrieve metadata for specified class
	**/
	public static function getMeta( phpClassName:String ) : Null<Dynamic> {
		ensureLoaded(phpClassName);
		return Global.isset(meta[phpClassName]) ? meta[phpClassName] : null;
	}

	/**
		Associate PHP class name with Haxe class name
	**/
	public static function registerClass( phpClassName:String, haxeClassName:String ) : Void {
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
	public static function getClass( phpClassName:String ) : HxClass {
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
	public static inline function getHxAnon() : HxClass {
		return cast HxAnon;
	}

	/**
		Returns Class<HxClass>
	**/
	public static inline function getHxClass() : HxClass {
		return cast HxClass;
	}

	/**
		Returns either Haxe class name for specified `phpClassName` or (if no such Haxe class registered) `phpClassName`.
	**/
	public static function getClassName( phpClassName:String ) : String {
		var hxClass = getClass(phpClassName);
		var name = getHaxeName(hxClass);
		return (name == null ? hxClass.phpClassName : name);
	}

	/**
		Returns original Haxe fully qualified class name for this type (if exists)
	**/
	public static function getHaxeName( hxClass:HxClass) : Null<String> {
		switch (hxClass.phpClassName) {
			case 'Int': return 'Int';
			case 'String': return 'String';
			case 'Bool': return 'Bool';
			case 'Float': return 'Float';
			case 'Class': return 'Class';
			case 'Enum': return 'Enum';
			case 'Dynamic': return 'Dynamic';
			case _:
		}

		inline function exists() return Global.isset(aliases[hxClass.phpClassName]);

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
	public static function getPhpName( haxeName:String ) : Null<String> {
		var prefix = getPrefix();
		var phpParts = (prefix.length == 0 ? [] : [prefix]);

		var haxeParts = haxeName.split('.');
		for (part in haxeParts) {
			switch (part.toLowerCase()) {
				case "__halt_compiler" | "abstract" | "and" | "array" | "as" | "break" | "callable" | "case" | "catch" | "class"
					| "clone" | "const" | "continue" | "declare" | "default" | "die" | "do" | "echo" | "else" | "elseif" | "empty"
					| "enddeclare" | "endfor" | "endforeach" | "endif" | "endswitch" | "endwhile" | "eval" | "exit" | "extends"
					| "final" | "finally" | "for" | "foreach" | "function" | "global" | "goto" | "if" | "implements" | "include"
					| "include_once" | "instanceof" | "insteadof" | "interface" | "isset" | "list" | "namespace" | "new" | "or"
					| "print" | "private" | "protected" | "public" | "require" | "require_once" | "return" | "static" | "switch"
					| "throw" | "trait" | "try" | "unset" | "use" | "var" | "while" | "xor" | "yield" | "__class__" | "__dir__"
					| "__file__" | "__function__" | "__line__" | "__method__" | "__trait__" | "__namespace__" | "int" | "float"
					| "bool" | "string" | "true" | "false" | "null" | "parent" | "void" | "iterable" | "object":
						part += '_hx';
				case _:
			}
			phpParts.push(part);
		}

		return phpParts.join('\\');
	}

	/**
		Creates Haxe-compatible closure.
		@param type `this` for instance methods; full php class name for static methods
		@param func Method name
	**/
	public static inline function closure( target:Dynamic, func:Dynamic ) : HxClosure {
		return new HxClosure(target, func);
	}

	/**
		Unsafe cast to HxClosure
	**/
	public static inline function castClosure(value:Dynamic) : HxClosure {
		return value;
	}

	/**
		Unsafe cast to HxClass
	**/
	public static inline function castClass(cls:Class<Dynamic>) : HxClass {
		return cast cls;
	}

	/**
		Returns `Class<T>` for `HxClosure`
	**/
	public static inline function closureHxClass() : HxClass {
		return cast HxClosure;
	}

	/**
		Implementation for `cast(value, Class<Dynamic>)`
		@throws HxException if `value` cannot be casted to this type
	**/
	public static function typedCast( hxClass:HxClass, value:Dynamic ) : Dynamic {
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
			case 'php\\NativeArray':
				if (value.is_array()) {
					return value;
				}
			case _:
				if (value.is_object() && Std.is(value, cast hxClass)) {
					return value;
				}
		}
		throw 'Cannot cast ' + Std.string(value) + ' to ' + getClassName(hxClass.phpClassName);
	}

	/**
		`trace()` implementation
	**/
	public static function trace( value:Dynamic, infos:PosInfos ) : Void {
		if (infos != null) {
			Global.echo('${infos.fileName}:${infos.lineNumber}: ');
		}
		Global.echo(stringify(value));
		if (infos.customParams != null) {
			for (value in infos.customParams) {
				Global.echo(',' + stringify(value));
			}
		}
		Global.echo('\n');
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
			return Syntax.string(value);
		}
		if (value.is_bool()) {
			return value ? 'true' : 'false';
		}
		if (value.is_array()) {
			var strings = Syntax.arrayDecl();
			Syntax.foreach(value, function(key:Dynamic, item:Dynamic) {
				Global.array_push(strings, (key:String) + ' => ' + stringify(item));
			});
			return '[' + Global.implode(', ', strings) + ']';
		}
		if (value.is_object()) {
			if (value.method_exists('toString')) {
				return value.toString();
			}
			if (value.method_exists('__toString')) {
				return value.__toString();
			}
			if (Std.is(value, StdClass)) {
				if (Global.isset(Syntax.getField(value, 'toString')) && value.toString.is_callable()) {
					return value.toString();
				}
				var result = new NativeIndexedArray<String>();
				var data = Global.get_object_vars(value);
				for (key in data.array_keys()) {
					result.array_push('$key : ' + stringify(data[key]));
				}
				return '{ ' + Global.implode(', ', result) + ' }';
			}
			if (isFunction(value)) {
				return '<function>';
			}
			if (Std.is(value, HxClass)) {
				return '[class ' + getClassName((value:HxClass).phpClassName) + ']';
			} else {
				return '[object ' + getClassName(Global.get_class(value)) + ']';
			}
		}
		throw "Unable to stringify value";
	}

	static public inline function isNumber( value:Dynamic ) {
		return value.is_int() || value.is_float();
	}

	/**
		Check if specified values are equal
	**/
	public static function equal( left:Dynamic, right:Dynamic ) : Bool {
		if (isNumber(left) && isNumber(right)) {
			return Syntax.binop(left, '==', right);
		}
		return Syntax.binop(left, '===', right);
	}

	/**
		Concat `left` and `right` if both are strings or string and null.
		Otherwise return sum of `left` and `right`.
	**/
	public static function addOrConcat( left:Dynamic, right:Dynamic ) : Dynamic {
		if (left.is_string() || right.is_string()) {
			return (left:String) + (right:String);
		}
		return Syntax.binop(left, '+', right);
	}

	/**
		`Std.is()` implementation
	**/
	public static function is( value:Dynamic, type:HxClass ) : Bool {
		if (type == null) return false;

		var phpType = type.phpClassName;
		switch (phpType) {
			case 'Dynamic':
				return true;
			case 'Int':
				return (
						value.is_int()
						|| (
							value.is_float()
							&& Syntax.binop(Syntax.int(value), '==', value)
							&& !Global.is_nan(value)
						)
					)
					&& Global.abs(value) <= 2147483648;
			case 'Float':
				return value.is_float() || value.is_int();
			case 'Bool':
				return value.is_bool();
			case 'String':
				return value.is_string();
			case 'php\\NativeArray', 'php\\_NativeArray\\NativeArray_Impl_':
				return value.is_array();
			case 'Enum', 'Class':
				if (Std.is(value, HxClass)) {
					var valuePhpClass = (cast value:HxClass).phpClassName;
					var enumPhpClass = (cast HxEnum:HxClass).phpClassName;
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
	public static inline function isClass(value:Dynamic) : Bool {
		return Std.is(value, HxClass);
	}

	/**
		Check if `value` is an enum constructor instance
	**/
	public static inline function isEnumValue(value:Dynamic) : Bool {
		return Std.is(value, HxEnum);
	}

	/**
		Check if `value` is a function
	**/
	public static inline function isFunction(value:Dynamic) : Bool {
		return Std.is(value, Closure) || Std.is(value, HxClosure);
	}

	/**
		Check if `value` is an instance of `HxClosure`
	**/
	public static inline function isHxClosure(value:Dynamic) : Bool {
		return Std.is(value, HxClosure);
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

	/**
		Helper method to avoid "Cannot use temporary expression in write context" error for expressions like this:
		```
		(new MyClass()).fieldName = 'value';
		```
	**/
	static public function deref( value:Dynamic ) : Dynamic {
		return value;
	}

	/**
		Create Haxe-compatible anonymous structure of `data` associative array
	**/
	static public inline function createAnon( data:NativeArray ) : Dynamic {
		return new HxAnon(data);
	}

	/**
		Make sure specified class is loaded
	**/
	static public inline function ensureLoaded(phpClassName:String ) : Bool {
		return Global.class_exists(phpClassName) || Global.interface_exists(phpClassName);
	}

	/**
		Get `field` of a dynamic `value` in a safe manner (avoid exceptions on trying to get a method)
	**/
	static public function dynamicField( value:Dynamic, field:String ) : Dynamic {
		if(Global.method_exists(value, field)) {
			return closure(value, field);
		}
		if(Global.is_string(value)) {
			value = @:privateAccess new HxDynamicStr(value);
		}
		return Syntax.getField(value, field);
	}
}


/**
	Class<T> implementation for Haxe->PHP internals.
**/
@:keep
@:dox(hide)
private class HxClass {

	public var phpClassName (default,null) : String;

	public function new( phpClassName:String ) : Void {
		this.phpClassName = phpClassName;
	}

	/**
		Magic method to call static methods of this class, when `HxClass` instance is in a `Dynamic` variable.
	**/
	@:phpMagic
	function __call( method:String, args:NativeArray ) : Dynamic {
		var callback = (phpClassName == 'String' ? (cast HxString:HxClass).phpClassName : phpClassName) + '::' + method;
		return Global.call_user_func_array(callback, args);
	}

	/**
		Magic method to get static vars of this class, when `HxClass` instance is in a `Dynamic` variable.
	**/
	@:phpMagic
	function __get( property:String ) : Dynamic {
		if (Global.defined('$phpClassName::$property')) {
			return Global.constant('$phpClassName::$property');
		} else if (Boot.hasGetter(phpClassName, property)) {
			return Syntax.staticCall(phpClassName, 'get_$property');
		} else {
			return Syntax.getStaticField(phpClassName, property);
		}
	}

	/**
		Magic method to set static vars of this class, when `HxClass` instance is in a `Dynamic` variable.
	**/
	@:phpMagic
	function __set( property:String, value:Dynamic ) : Void {
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
private class HxEnum {
	static var singletons = new Map<String,HxEnum>();

	var tag : String;
	var index : Int;
	var params : NativeArray;

	/**
		Returns instances of constructors without arguments
	**/
	public static function singleton( enumClass:String, tag:String, index:Int ) : HxEnum {
		var key = '$enumClass::$tag';

		var instance = singletons.get(key);
		if (instance == null) {
			instance = Syntax.construct(enumClass, tag, index);
			singletons.set(key, instance);
		}

		return instance;
	}

	public function new( tag:String, index:Int, arguments:NativeArray = null ) : Void {
		this.tag = tag;
		this.index = index;
		params = (arguments == null ? new NativeArray() : arguments);
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
	@:phpMagic
	public function __toString() : String {
		var result = tag;
		if (Global.count(params) > 0) {
			var strings = Global.array_map(function (item) return Boot.stringify(item), params);
			result += '(' + Global.implode(',', strings) + ')';
		}
		return result;
	}
}


/**
	`String` implementation
**/
@:keep
@:dox(hide)
private class HxString {

	public static function toUpperCase( str:String ) : String {
		return Global.strtoupper(str);
	}

	public static function toLowerCase( str:String ) : String {
		return Global.strtolower(str);
	}

	public static function charAt( str:String, index:Int) : String {
		if (index < 0 || index >= str.length) {
			return '';
		} else {
			return (str:NativeString)[index];
		}
	}

	public static function charCodeAt( str:String, index:Int) : Null<Int> {
		if (index < 0 || index >= str.length) {
			return null;
		} else {
			return Global.ord((str:NativeString)[index]);
		}
	}

	public static function indexOf( str:String, search:String, startIndex:Int = null ) : Int {
		if (startIndex == null) {
			startIndex = 0;
		} else if (startIndex < 0) {
			startIndex += str.length;
		}
		var index = Global.strpos(str, search, startIndex);
		return (index == false ? -1 : index);
	}

	public static function lastIndexOf( str:String, search:String, startIndex:Int = null ) : Int {
		var index = Global.strrpos(str, search, (startIndex == null ? 0 : startIndex - str.length));
		if (index == false) {
			return -1;
		} else {
			return index;
		}
	}

	public static function split( str:String, delimiter:String ) : Array<String> {
		if (delimiter == '') {
			return @:privateAccess Array.wrap(Global.str_split(str));
		} else {
			return @:privateAccess Array.wrap(Global.explode(delimiter, str));
		}
	}

	public static function substr( str:String, pos:Int, ?len:Int ) : String {
		if (pos < -str.length) {
			pos = 0;
		} else if (pos >= str.length) {
			return '';
		}
		if (len == null) {
			return Global.substr(str, pos);
		} else {
			var result = Global.substr(str, pos, len);
			return (result == false ? '' : result);
		}
	}

	public static function substring( str:String, startIndex:Int, ?endIndex:Int ) : String {
		if (endIndex == null) {
			endIndex = str.length;
		} else if (endIndex < 0) {
			endIndex = 0;
		}
		if (startIndex < 0) startIndex = 0;
		if (startIndex > endIndex) {
			var tmp = endIndex;
			endIndex = startIndex;
			startIndex = tmp;
		}
		var result = Global.substr(str, startIndex, endIndex - startIndex);
		return (result == false ? '' : result);
	}

	public static function toString( str:String ) : String {
		return str;
	}

	public static function fromCharCode( code:Int ) : String {
		return Global.chr(code);
	}
}

/**
	For Dynamic access which looks like String
**/
@:dox(hide)
@:keep
private class HxDynamicStr {
	static var hxString : String = (cast HxString:HxClass).phpClassName;
	var str : String;

	/**
		Returns HxDynamicStr instance if `value` is a string.
		Otherwise returns `value` as-is.
	**/
	static function wrap( value:Dynamic ) : Dynamic {
		if (value.is_string()) {
			return new HxDynamicStr(value);
		} else {
			return value;
		}
	}

	function new( str:String ) {
		this.str = str;
	}

	@:phpMagic
	function __get( field:String ) : Dynamic {
		switch (field) {
			case 'length':      return str.length;
			case 'toUpperCase': return HxString.toUpperCase.bind(str);
			case 'toLowerCase': return HxString.toLowerCase.bind(str);
			case 'charAt':      return HxString.charAt.bind(str);
			case 'indexOf':     return HxString.indexOf.bind(str);
			case 'lastIndexOf': return HxString.lastIndexOf.bind(str);
			case 'split':       return HxString.split.bind(str);
			case 'toString':    return HxString.toString.bind(str);
			case 'substring':   return HxString.substring.bind(str);
			case 'substr':      return HxString.substr.bind(str);
			case 'charCodeAt':  return HxString.charCodeAt.bind(str);
		}
		/** Force invalid field access error */
		return Syntax.getField(str, field);
	}

	@:phpMagic
	function __call( method:String, args:NativeArray ) : Dynamic {
		Global.array_unshift(args, str);
		return Global.call_user_func_array(hxString + '::' + method, args);
	}
}


/**
	Anonymous objects implementation
**/
@:keep
@:dox(hide)
private class HxAnon extends StdClass {

	public function new( fields:NativeArray = null ) {
		super();
		if (fields != null) {
			Syntax.foreach(fields, function(name, value) Syntax.setField(this, name, value));
		}
	}

	@:phpMagic
	function __get( name:String ) {
		return null;
	}

	@:phpMagic
	function __call( name:String, args:NativeArray ) : Dynamic {
		var method = Syntax.getField(this, name);
		Syntax.keepVar(method);
		return method(Syntax.splat(args));
	}
}

/**
	Closures implementation
**/
@:keep
@:dox(hide)
private class HxClosure {
	/** `this` for instance methods; php class name for static methods */
	var target : Dynamic;
	/** Method name for methods */
	var func : String;

	public function new( target:Dynamic, func:String ) : Void {
		this.target = target;
		this.func = func;
		//Force runtime error if trying to create a closure of an instance which happen to be `null`
		if (target.is_null()) {
			throw "Unable to create closure on `null`";
		}
	}

	/**
		@see http://php.net/manual/en/language.oop5.magic.php#object.invoke
	**/
	@:phpMagic
	public function __invoke() {
		return Global.call_user_func_array(getCallback(), Global.func_get_args());
	}

	/**
		Generates callable value for PHP
	**/
	public function getCallback(eThis:Dynamic = null) : NativeIndexedArray<Dynamic> {
		if (eThis == null) {
			eThis = target;
		}
		if (Std.is(eThis, StdClass)) {
			if (Std.is(eThis, HxAnon)) {
				return Syntax.getField(eThis, func);
			}
		}
		return Syntax.arrayDecl(eThis, func);
	}

	/**
		Check if this is the same closure
	**/
	public function equals( closure:HxClosure ) : Bool {
		return (target == closure.target && func == closure.func);
	}

	/**
		Invoke this closure with `newThis` instead of `this`
	**/
	public function callWith( newThis:Dynamic, args:NativeArray ) : Dynamic {
		return Global.call_user_func_array(getCallback(newThis), args);
	}
}

/**
	Special exception which is used to wrap non-throwable values
**/
@:keep
@:dox(hide)
private class HxException extends Exception {
  var e : Dynamic;
  public function new( e:Dynamic ) : Void {
	  this.e = e;
	  super(Boot.stringify(e));
  }
}