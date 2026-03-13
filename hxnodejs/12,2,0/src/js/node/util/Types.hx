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

package js.node.util;

/**
	`util.types` provides a number of type checks for different kinds of built-in objects.

	@see https://nodejs.org/api/util.html#util_util_types
**/
@:jsRequire("util", "types")
extern class Types {
	/**
		Returns `true` if the value is a built-in `ArrayBuffer` or `SharedArrayBuffer` instance.

		@see https://nodejs.org/api/util.html#util_util_types_isanyarraybuffer_value
	**/
	static function isAnyArrayBuffer(value:Dynamic):Bool;

	/**
		Returns `true` if the value is an `arguments` object.

		@see https://nodejs.org/api/util.html#util_util_types_isargumentsobject_value
	**/
	static function isArgumentsObject(value:Dynamic):Bool;

	/**
		Returns `true` if the value is a built-in `ArrayBuffer` instance.

		@see https://nodejs.org/api/util.html#util_util_types_isarraybuffer_value
	**/
	static function isArrayBuffer(value:Dynamic):Bool;

	/**
		Returns `true` if the value is an async function.

		@see https://nodejs.org/api/util.html#util_util_types_isasyncfunction_value
	**/
	static function isAsyncFunction(value:Dynamic):Bool;

	/**
		Returns `true` if the value is a `BigInt64Array` instance.

		@see https://nodejs.org/api/util.html#util_util_types_isbigint64array_value
	**/
	static function isBigInt64Array(value:Dynamic):Bool;

	/**
		Returns `true` if the value is a `BigUint64Array` instance.

		@see https://nodejs.org/api/util.html#util_util_types_isbiguint64array_value
	**/
	static function isBigUint64Array(value:Dynamic):Bool;

	/**
		Returns `true` if the value is a boolean object, e.g. created by `new Boolean()`.

		@see https://nodejs.org/api/util.html#util_util_types_isbooleanobject_value
	**/
	static function isBooleanObject(value:Dynamic):Bool;

	/**
		Returns `true` if the value is any boxed primitive object, e.g. created by `new Boolean()`, `new String()` or
		`Object(Symbol())`.

		@see https://nodejs.org/api/util.html#util_util_types_isboxedprimitive_value
	**/
	static function isBoxedPrimitive(value:Dynamic):Bool;

	/**
		Returns `true` if the value is a built-in `DataView` instance.

		@see https://nodejs.org/api/util.html#util_util_types_isdataview_value
	**/
	static function isDataView(value:Dynamic):Bool;

	/**
		Returns `true` if the value is a built-in `Date` instance.

		@see https://nodejs.org/api/util.html#util_util_types_isdate_value
	**/
	static function isDate(value:Dynamic):Bool;

	/**
		Returns `true` if the value is a native `External` value.

		@see https://nodejs.org/api/util.html#util_util_types_isexternal_value
	**/
	static function isExternal(value:Dynamic):Bool;

	/**
		Returns `true` if the value is a built-in `Float32Array` instance.

		@see https://nodejs.org/api/util.html#util_util_types_isfloat32array_value
	**/
	static function isFloat32Array(value:Dynamic):Bool;

	/**
		Returns `true` if the value is a built-in `Float64Array` instance.

		@see https://nodejs.org/api/util.html#util_util_types_isfloat64array_value
	**/
	static function isFloat64Array(value:Dynamic):Bool;

	/**
		Returns `true` if the value is a generator function.

		@see https://nodejs.org/api/util.html#util_util_types_isgeneratorfunction_value
	**/
	static function isGeneratorFunction(value:Dynamic):Bool;

	/**
		Returns `true` if the value is a generator object as returned from a built-in generator function.

		@see https://nodejs.org/api/util.html#util_util_types_isgeneratorobject_value
	**/
	static function isGeneratorObject(value:Dynamic):Bool;

	/**
		Returns `true` if the value is a built-in `Int8Array` instance.

		@see https://nodejs.org/api/util.html#util_util_types_isint8array_value
	**/
	static function isInt8Array(value:Dynamic):Bool;

	/**
		Returns `true` if the value is a built-in `Int16Array` instance.

		@see https://nodejs.org/api/util.html#util_util_types_isint16array_value
	**/
	static function isInt16Array(value:Dynamic):Bool;

	/**
		Returns `true` if the value is a built-in `Int32Array` instance.

		@see https://nodejs.org/api/util.html#util_util_types_isint32array_value
	**/
	static function isInt32Array(value:Dynamic):Bool;

	/**
		Returns `true` if the value is a built-in `Map` instance.

		@see https://nodejs.org/api/util.html#util_util_types_ismap_value
	**/
	static function isMap(value:Dynamic):Bool;

	/**
		Returns `true` if the value is an iterator returned for a built-in `Map` instance.

		@see https://nodejs.org/api/util.html#util_util_types_ismapiterator_value
	**/
	static function isMapIterator(value:Dynamic):Bool;

	/**
		Returns `true` if the value is an instance of a Module Namespace Object.

		@see https://nodejs.org/api/util.html#util_util_types_ismodulenamespaceobject_value
	**/
	static function isModuleNamespaceObject(value:Dynamic):Bool;

	/**
		Returns `true` if the value is an instance of a built-in `Error` type.

		@see https://nodejs.org/api/util.html#util_util_types_isnativeerror_value
	**/
	static function isNativeError(value:Dynamic):Bool;

	/**
		Returns `true` if the value is a number object, e.g. created by `new Number()`.

		@see https://nodejs.org/api/util.html#util_util_types_isnumberobject_value
	**/
	static function isNumberObject(value:Dynamic):Bool;

	/**
		Returns `true` if the value is a built-in `Promise`.

		@see https://nodejs.org/api/util.html#util_util_types_ispromise_value
	**/
	static function isPromise(value:Dynamic):Bool;

	/**
		Returns `true` if the value is a `Proxy` instance.

		@see https://nodejs.org/api/util.html#util_util_types_isproxy_value
	**/
	static function isProxy(value:Dynamic):Bool;

	/**
		Returns `true` if the value is a regular expression object.

		@see https://nodejs.org/api/util.html#util_util_types_isregexp_value
	**/
	static function isRegExp(value:Dynamic):Bool;

	/**
		Returns `true` if the value is a built-in `Set` instance.

		@see https://nodejs.org/api/util.html#util_util_types_isset_value
	**/
	static function isSet(value:Dynamic):Bool;

	/**
		Returns `true` if the value is an iterator returned for a built-in `Set` instance.

		@see https://nodejs.org/api/util.html#util_util_types_issetiterator_value
	**/
	static function isSetIterator(value:Dynamic):Bool;

	/**
		Returns `true` if the value is a built-in `SharedArrayBuffer` instance.

		@see https://nodejs.org/api/util.html#util_util_types_issharedarraybuffer_value
	**/
	static function isSharedArrayBuffer(value:Dynamic):Bool;

	/**
		Returns `true` if the value is a string object, e.g. created by `new String()`.

		@see https://nodejs.org/api/util.html#util_util_types_isstringobject_value
	**/
	static function isStringObject(value:Dynamic):Bool;

	/**
		Returns `true` if the value is a symbol object, created by calling `Object()` on a `Symbol` primitive.

		@see https://nodejs.org/api/util.html#util_util_types_issymbolobject_value
	**/
	static function isSymbolObject(value:Dynamic):Bool;

	/**
		Returns `true` if the value is a built-in `TypedArray` instance.

		@see https://nodejs.org/api/util.html#util_util_types_istypedarray_value
	**/
	static function isTypedArray(value:Dynamic):Bool;

	/**
		Returns `true` if the value is a built-in `Uint8Array` instance.

		@see https://nodejs.org/api/util.html#util_util_types_isuint8array_value
	**/
	static function isUint8Array(value:Dynamic):Bool;

	/**
		Returns `true` if the value is a built-in `Uint8ClampedArray` instance.

		@see https://nodejs.org/api/util.html#util_util_types_isuint8clampedarray_value
	**/
	static function isUint8ClampedArray(value:Dynamic):Bool;

	/**
		Returns `true` if the value is a built-in `Uint16Array` instance.

		@see https://nodejs.org/api/util.html#util_util_types_isuint16array_value
	**/
	static function isUint16Array(value:Dynamic):Bool;

	/**
		Returns `true` if the value is a built-in `Uint32Array` instance.

		@see https://nodejs.org/api/util.html#util_util_types_isuint32array_value
	**/
	static function isUint32Array(value:Dynamic):Bool;

	/**
		Returns `true` if the value is a built-in `WeakMap` instance.

		@see https://nodejs.org/api/util.html#util_util_types_isweakmap_value
	**/
	static function isWeakMap(value:Dynamic):Bool;

	/**
		Returns `true` if the value is a built-in `WeakSet` instance.

		@see https://nodejs.org/api/util.html#util_util_types_isweakset_value
	**/
	static function isWeakSet(value:Dynamic):Bool;

	/**
		Returns `true` if the value is a built-in `WebAssembly.Module` instance.

		@see https://nodejs.org/api/util.html#util_util_types_iswebassemblycompiledmodule_value
	**/
	static function isWebAssemblyCompiledModule(value:Dynamic):Bool;
}
