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
package haxe;

/**
	A typed interface for bit flags. This is not a real object, only a typed
	interface for an actual Int. Each flag can be tested/set with the
	corresponding enum instance. Up to 32 flags can be stored that way.

	Enum constructor indices are preserved from Haxe syntax, so the first
	declared is index 0, the next index 1 etc. The methods are optimized if the
	enum instance is passed directly, e.g. as has(EnumCtor). Otherwise
	Type.enumIndex() reflection is used.
**/
abstract EnumFlags<T:EnumValue>(Int) {

	/**
		Initializes the bitflags to `i`.
	**/
	public inline function new(i = 0) {
		this = i;
	}

	/**
		Checks if the index of enum instance `v` is set.

		This method is optimized if `v` is an enum instance expression such as
		SomeEnum.SomeCtor.

		If `v` is null, the result is unspecified.
	**/
	public inline function has( v : T ) : Bool {
		return this & (1 << Type.enumIndex(v)) != 0;
	}

	/**
		Sets the index of enum instance `v`.

		This method is optimized if `v` is an enum instance expression such as
		SomeEnum.SomeCtor.

		If `v` is null, the result is unspecified.
	**/
	public inline function set( v : T ) : Void {
		this |= 1 << Type.enumIndex(v);
	}

	/**
		Unsets the index of enum instance `v`.

		This method is optimized if `v` is an enum instance expression such as
		SomeEnum.SomeCtor.

		If `v` is null, the result is unspecified.
	**/
	public inline function unset( v : T ) : Void {
		this &= 0xFFFFFFFF - (1 << Type.enumIndex(v));
	}

	/**
		Convert a integer bitflag into a typed one (this is a no-op, it does not
		have any impact on speed).
	**/
	public inline static function ofInt<T:EnumValue>( i : Int ) : EnumFlags<T> {
		return new EnumFlags<T>(i);
	}

	/**
		Convert the typed bitflag into the corresponding int value (this is a
		no-op, it doesn't have any impact on speed).
	**/
	public inline function toInt() : Int {
		return this;
	}
}
