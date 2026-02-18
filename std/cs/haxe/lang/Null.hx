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

package haxe.lang;

/**
	Wrapper struct for nullable values that provides unified semantics
	across both value types and reference types.

	Unlike C#'s native `Nullable<T>` which only works for value types,
	this wrapper provides:
	- Unified API using `.hasValue` and `.value` for any type
	- Implicit conversion to/from `T`
	- Proper arithmetic behavior (Null<int> + int → int)
**/
@:include("haxe/lang/Null.cs")
extern class Null<T> {
	/**
		The wrapped value. Returns `default(T)` if `hasValue` is false.
	**/
	public var value(default, null):T;

	/**
		Whether this nullable contains a valid value.
	**/
	public var hasValue(default, null):Bool;

	/**
		Creates a new Null<T> wrapper.
	**/
	public function new(v:T, hasValue:Bool):Void;

	/**
		Converts this nullable to a dynamic object.
		Returns the boxed value if hasValue is true, otherwise null.
	**/
	public function toDynamic():Dynamic;

	/**
		Creates a Null<T> from a dynamic value with proper type conversion.
	**/
	public static function ofDynamic<D>(obj:Dynamic):Null<D>;

	/**
		Alternative static method for creating from dynamic (used internally).
	**/
	@:native("_ofDynamic")
	public static function _ofDynamic<D>(obj:Dynamic):Null<D>;
}
