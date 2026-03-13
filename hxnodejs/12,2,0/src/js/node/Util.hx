/*
 * Copyright (C)2014-2020 Haxe Foundation
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

package js.node;

import haxe.Constraints.Function;
import haxe.extern.EitherType;
import haxe.extern.Rest;
import js.node.stream.Readable;
import js.node.stream.Writable;
#if haxe4
import js.lib.Error;
import js.lib.Promise;
#else
import js.Error;
import js.Promise;
#end

/**
	The `util` module is primarily designed to support the needs of Node.js' own internal APIs.

	@see https://nodejs.org/api/util.html#util_util
**/
@:jsRequire("util")
extern class Util {
	/**
		Takes an `async` function (or a function that returns a `Promise`) and returns a function following the
		error-first callback style, i.e. taking an `(err, value) => ...` callback as the last argument.

		@see https://nodejs.org/api/util.html#util_util_callbackify_original
	**/
	static function callbackify(original:Function, args:Rest<Dynamic>):Null<Error>->Null<Dynamic>->Void;

	/**
		The `util.debuglog()` method is used to create a function that conditionally writes debug messages to `stderr`
		based on the existence of the `NODE_DEBUG` environment variable.

		@see https://nodejs.org/api/util.html#util_util_debuglog_section
	**/
	static function debuglog(section:String):Rest<Dynamic>->Void;

	/**
		The `util.deprecate()` method wraps `fn` (which may be a function or class) in such a way that it is marked
		asdeprecated.

		@see https://nodejs.org/api/util.html#util_util_deprecate_fn_msg_code
	**/
	static function deprecate<T:haxe.Constraints.Function>(fun:T, msg:String, ?code:String):T;

	/**
		The `util.format()` method returns a formatted string using the first argument as a `printf`-like format string
		which can contain zero or more format specifiers.

		@see https://nodejs.org/api/util.html#util_util_format_format_args
	**/
	@:overload(function(args:Rest<Dynamic>):String {})
	static function format(format:String, args:Rest<Dynamic>):String;

	/**
		This function is identical to `util.format()`, except in that it takes an `inspectOptions` argument which
		specifies options that are passed along to `util.inspect()`.

		@see https://nodejs.org/api/util.html#util_util_formatwithoptions_inspectoptions_format_args
	**/
	@:overload(function(inspectOptions:InspectOptions, args:Rest<Dynamic>):String {})
	static function formatWithOptions(inspectOptions:InspectOptions, format:String, args:Rest<Dynamic>):String;

	/**
		Returns the string name for a numeric error code that comes from a Node.js API.

		@see https://nodejs.org/api/util.html#util_util_getsystemerrorname_err
	**/
	static function getSystemErrorName(err:Int):String;

	/**
		Inherit the prototype methods from one `constructor` into another.

		@see https://nodejs.org/api/util.html#util_util_inherits_constructor_superconstructor
	**/
	@:deprecated
	static function inherits(constructor:Class<Dynamic>, superConstructor:Class<Dynamic>):Void;

	/**
		The `util.inspect()` method returns a string representation of `object` that is intended for debugging.

		@see https://nodejs.org/api/util.html#util_util_inspect_object_options
	**/
	@:overload(function(object:Dynamic, ?showHidden:Bool, ?depth:Int, ?colors:Bool):String {})
	static function inspect(object:Dynamic, ?options:InspectOptions):String;

	/**
		Returns `true` if there is deep strict equality between `val1` and `val2`.

		@see https://nodejs.org/api/util.html#util_util_isdeepstrictequal_val1_val2
	**/
	static function isDeepStrictEqual(val1:Dynamic, val2:Dynamic):Bool;

	/**
		Takes a function following the common error-first callback style, i.e. taking an `(err, value) => ...` callback
		as the last argument, and returns a version that returns promises.

		@see https://nodejs.org/api/util.html#util_util_promisify_original
	**/
	static function promisify(original:Function):Rest<Dynamic>->Promise<Dynamic>;

	/**
		Deprecated predecessor of `Console.error`.
	**/
	@:deprecated("Use js.Node.console.error instead")
	static function debug(string:String):Void;

	/**
		Deprecated predecessor of console.error.
	**/
	@:deprecated("Use js.Node.console.error instead")
	static function error(args:Rest<Dynamic>):Void;

	/**
		Returns true if the given "object" is an Array. false otherwise.
	**/
	@:deprecated
	static function isArray(object:Dynamic):Bool;

	/**
		Returns true if the given "object" is a Bool. false otherwise.
	**/
	@:deprecated
	static function isBoolean(object:Dynamic):Bool;

	/**
		Returns true if the given "object" is a Buffer. false otherwise.
	**/
	@:deprecated
	static function isBuffer(object:Dynamic):Bool;

	/**
		Returns true if the given "object" is a Date. false otherwise.
	**/
	@:deprecated
	static function isDate(object:Dynamic):Bool;

	/**
		Returns true if the given "object" is an Error. false otherwise.
	**/
	@:deprecated
	static function isError(object:Dynamic):Bool;

	/**
		Returns true if the given "object" is a Function. false otherwise.
	**/
	@:deprecated
	static function isFunction(object:Dynamic):Bool;

	/**
		Returns true if the given "object" is strictly null. false otherwise.
	**/
	@:deprecated
	static function isNull(object:Dynamic):Bool;

	/**
		Returns true if the given "object" is null or undefined. false otherwise.
	**/
	@:deprecated
	static function isNullOrUndefined(object:Dynamic):Bool;

	/**
		Returns true if the given "object" is a Float. false otherwise.
	**/
	@:deprecated
	static function isNumber(object:Dynamic):Bool;

	/**
		Returns true if the given "object" is strictly an Object and not a Function. false otherwise.
	**/
	@:deprecated
	static function isObject(object:Dynamic):Bool;

	/**
		Returns true if the given "object" is a primitive type. false otherwise.
	**/
	@:deprecated
	static function isPrimitive(object:Dynamic):Bool;

	/**
		Returns true if the given "object" is a RegExp. false otherwise.
	**/
	@:deprecated
	static function isRegExp(object:Dynamic):Bool;

	/**
		Returns true if the given "object" is a String. false otherwise.
	**/
	@:deprecated
	static function isString(object:Dynamic):Bool;

	/**
		Returns true if the given "object" is a Symbol. false otherwise.
	**/
	@:deprecated
	static function isSymbol(object:Dynamic):Bool;

	/**
		Returns true if the given "object" is undefined. false otherwise.
	**/
	@:deprecated
	static function isUndefined(object:Dynamic):Bool;

	/**
		Output with timestamp on stdout.
	**/
	@:deprecated
	static function log(args:Rest<Dynamic>):Void;

	/**
		Deprecated predecessor of console.log.
	**/
	@:deprecated("Use js.Node.console.log instead")
	static function print(args:Rest<Dynamic>):Void;

	/**
		Deprecated predecessor of console.log.
	**/
	@:deprecated("Use js.Node.console.log instead")
	static function puts(args:Rest<Dynamic>):Void;

	/**
		Deprecated predecessor of stream.pipe().
	**/
	@:deprecated("Use `readableStream.pipe(writableStream)` instead")
	static function pump(readableStream:IReadable, writableStream:IWritable, ?callback:Error->Void):Void;
}

/**
	Options object used by `Console.dir`.
**/
typedef InspectOptionsBase = {
	/**
		If `true`, `object`'s non-enumerable symbols and properties are included in the formatted result.
		`WeakMap` and `WeakSet` entries are also included.

		Default: `false`.
	**/
	@:optional var showHidden:Bool;

	/**
		Specifies the number of times to recurse while formatting `object`.
		This is useful for inspecting large objects. To recurse up to the maximum call stack size pass `Infinity` or
		`null`.

		Default: `2`.
	**/
	@:optional var depth:Null<Int>;

	/**
		If `true`, the output is styled with ANSI color codes.
		Colors are customizable.
		See Customizing `util.inspect` colors.

		Default: `false`.
	**/
	@:optional var colors:Bool;
}

/**
	Options object used by `Util.inspect`.
**/
typedef InspectOptions = {
	> InspectOptionsBase,

	/**
		If `false`, `[util.inspect.custom](depth, opts)` functions are not invoked.

		Default: `true`.
	**/
	@:optional var customInspect:Bool;

	/**
		If `true`, `Proxy` inspection includes the `target` and `handler` objects.

		Default: `false`.
	**/
	@:optional var showProxy:Bool;

	/**
		Specifies the maximum number of `Array`, `TypedArray`, `WeakMap` and `WeakSet` elements to include when
		formatting.
		Set to `null` or `Infinity` to show all elements.
		Set to `0` or negative to show no elements.

		Default: `100`.
	**/
	@:optional var maxArrayLength:Null<Int>;

	/**
		The length at which input values are split across multiple lines.
		Set to `Infinity` to format the input as a single line (in combination with `compact` set to `true` or any
		number >= `1`).

		Default: `80`.
	**/
	@:optional var breakLength:Float;

	/**
		Setting this to `false` causes each object key to be displayed on a new line.
		It will also add new lines to text that is longer than `breakLength`.
		If set to a number, the most `n` inner elements are united on a single line as long as all properties fit into
		`breakLength`.
		Short array elements are also grouped together.
		No text will be reduced below 16 characters, no matter the `breakLength` size.
		For more information, see the example below.

		Default: `3`.
	**/
	@:optional var compact:EitherType<Bool, Int>;

	/**
		If set to `true` or a function, all properties of an object, and `Set` and `Map` entries are sorted in the
		resulting string.
		If set to `true` the default sort is used.
		If set to a function, it is used as a compare function.
	**/
	@:optional var sorted:EitherType<Bool, Dynamic->Dynamic->Int>;

	/**
		If set to `true`, getters are inspected.
		If set to `'get'`, only getters without a corresponding setter are inspected.
		If set to `'set'`, only getters with a corresponding setter are inspected.
		This might cause side effects depending on the getter function.

		Default: `false`.
	**/
	@:optional var getters:EitherType<Bool, String>;
}
