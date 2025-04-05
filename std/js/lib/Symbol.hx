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

package js.lib;

@:native("Symbol")
extern class Symbol {
	/**
		To create a new primitive symbol, use `new Symbol()` with an optional string as its `description`.
		NOTE: Unlike in plain JavaScript, `new Symbol()` syntax is used in Haxe. This generates a `Symbol(...)`
		expression as required by the JavaScript specification.

		See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Symbol
	**/
	@:pure @:selfCall function new(?description:String);

	/**
		The Symbol.for(key) method searches for existing symbols in a
		runtime-wide symbol registry with the given key and returns it if found.
		Otherwise a new symbol gets created in the global symbol registry with
		this key.

		See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Symbol/for
	**/
	@:native("for") static function for_(key:String):Symbol;

	/**
		The Symbol.keyFor(sym) method retrieves a shared symbol key from the
		global symbol registry for the given symbol.

		See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Symbol/keyFor
	**/
	@:pure static function keyFor(sym:Symbol):Null<String>;

	/**
		Returns a string containing the description of the Symbol.
	**/
	@:pure function toString():String;

	/**
		A method returning the default iterator for an object.
	**/
	static var iterator(default, null):Symbol;

	/**
		A method that returns the default AsyncIterator for an object.
	**/
	static var asyncIterator(default, null):Symbol;

	/**
		A method that matches against a string, also used to determine if an
		object may be used as a regular expression. Used by
		String.prototype.match().
	**/
	static var match(default, null):Symbol;

	/**
		A method that replaces matched substrings of a string. Used by
		String.prototype.replace().
	**/
	static var replace(default, null):Symbol;

	/**
		A method that returns the index within a string that matches the regular
		expression. Used by String.prototype.search().
	**/
	static var search(default, null):Symbol;

	/**
		A method that splits a string at the indices that match a regular
		expression. Used by String.prototype.split().
	**/
	static var split(default, null):Symbol;

	/**
		A method determining if a constructor object recognizes an object as its
		instance. Used by instanceof.
	**/
	static var hasInstance(default, null):Symbol;

	/**
		A Boolean value indicating if an object should be flattened to its array
		elements. Used by Array.prototype.concat().
	**/
	static var isConcatSpreadable(default, null):Symbol;

	/**
		An object value of whose own and inherited property names are excluded
		from the with environment bindings of the associated object.
	**/
	static var unscopables(default, null):Symbol;

	/**
		A constructor function that is used to create derived objects.
	**/
	static var species(default, null):Symbol;

	/**
		A method converting an object to a primitive value.
	**/
	static var toPrimitive(default, null):Symbol;

	/**
		A string value used for the default description of an object. Used by
		Object.prototype.toString().
	**/
	static var toStringTag(default, null):Symbol;

	/**
		Retrieve symbol from a given `object`.

		NOTE: This is a Haxe-specific method that generates an `object[symbol]` expression.
	**/
	inline function ofObject<T>(object:{}):Null<T>
		return (cast object)[cast this];
}
