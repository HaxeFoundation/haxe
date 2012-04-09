/*
 * Copyright (c) 2005, The haXe Project Contributors
 * All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   - Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   - Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE HAXE PROJECT CONTRIBUTORS "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE HAXE PROJECT CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
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
