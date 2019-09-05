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
	**/
	@:pure @:selfCall function new(?description:String);

	/**
		Searches for existing symbols with the given key and returns it if found.
		Otherwise a new symbol gets created in the global symbol registry with this key.
	**/
	@:native("for") static function for_(key:String):Symbol;

	/**
		Retrieves a shared symbol key from the global symbol registry for the given symbol.
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
		Retrieve symbol from a given `object`.

		NOTE: This is a Haxe-specific method that generates an `object[symbol]` expression.
	**/
	inline function ofObject<T>(object:{}):Null<T>
		return (cast object)[cast this];
}
