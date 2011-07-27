/*
 * Copyright (c) 2005-2011, The haXe Project Contributors
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
package neko.db;

// basic types
typedef SId = Int
typedef SInt = Int
typedef SUId = Int
typedef SUInt = Int
typedef SBigId = Float
typedef SBigInt = Float
typedef SSingle = Float
typedef SFloat = Float
typedef SBool = Bool
typedef SString<Const> = String
typedef SDate = Date
typedef SDateTime = Date
typedef STinyText = String
typedef SSmallText = String
typedef SText = String
typedef SSmallBinary = String
typedef SLongBinary = String
typedef SBinary = String
typedef SBytes<Const> = String
typedef STinyInt = Int

// extra
typedef SNull<T> = T
typedef SEncoded = Int
typedef SSerialized = String
typedef SNekoSerialized = String

@:native("Int")
extern class SFlags<T> {
	public inline function get( v : T ) : Bool {
		return (cast this) & (1 << Type.enumIndex(v)) != 0;
	}
	public inline function set( v : T ) : Void {
		untyped __this__ |= 1 << Type.enumIndex(v);
	}
	public inline function unset( v : T ) : Void {
		untyped __this__ &= 0xFFFFFFF - (1 << Type.enumIndex(v));
	}
	public inline static function ofInt<T>( i : Int ) : SFlags<T> {
		return cast i;
	}
	public inline function toInt() : Int {
		return cast this;
	}
}
