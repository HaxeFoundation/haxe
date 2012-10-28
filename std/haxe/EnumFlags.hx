/*
 * Copyright (C)2005-2012 Haxe Foundation
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
	A typed interface for bit flags. This is not a real object, only a typed interface for an actual Int. Each flag can be tested/set with the corresponding enum value. Up to 32 flags can be stored that way.
**/
@:native("Int")
extern class EnumFlags<T:EnumValue> {

	/**
		Initialize the bitflags (set it to 0)
	**/
	public inline function init() : Void {
		untyped __this__ = 0;
	}

	/**
		Check if the bitflag has the corresponding enum set. This will be compiled as a single bit & mask != 0 in case the enum value is directly passed to the method.
	**/
	public inline function has( v : T ) : Bool {
		return (cast this) & (1 << Type.enumIndex(cast v)) != 0;
	}

	/**
		Set the bitflag for the corresponding enum value.
	**/
	public inline function set( v : T ) : Void {
		untyped __this__ |= 1 << Type.enumIndex(cast v);
	}

	/**
		Unset the bitflag for the corresponding enum value.
	**/
	public inline function unset( v : T ) : Void {
		untyped __this__ &= 0xFFFFFFF - (1 << Type.enumIndex(cast v));
	}

	/**
		Convert a integer bitflag into a typed one (this is a no-op, it doesn't have any impact on speed).
	**/
	public inline static function ofInt<T:EnumValue>( i : Int ) : EnumFlags<T> {
		return cast i;
	}

	/**
		Convert the typed bitflag into the corresponding int value (this is a no-op, it doesn't have any impact on speed).
	**/
	public inline function toInt() : Int {
		return cast this;
	}
}
