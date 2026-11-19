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

package sys;

#if (hl || jvm || cpp || eval)
private typedef FilePosData = haxe.Int64;
#else
private typedef FilePosData = Float;
#end

/**
	A position or offset in a file, or a file size, in bytes.

	The underlying type is the best one the target provides for file positions:
	`haxe.Int64` on hl, jvm, cpp and eval, and the native number on the other
	targets. Integer values are exact at least up to 2^53 on all targets.

	The runtime value therefore differs between targets, for example through
	`Dynamic` or `haxe.Serializer`. Use `toFloat` or `toString` to store a
	`FilePos` portably.
**/
abstract FilePos(FilePosData) {
	inline function new(v:FilePosData)
		this = v;

	@:from public static inline function ofInt(v:Int):FilePos
		return new FilePos(v);

	public static inline function ofInt64(v:haxe.Int64):FilePos {
		#if (hl || jvm || cpp || eval)
		return new FilePos(v);
		#else
		return new FilePos(v.toFloat());
		#end
	}

	/**
		Creates a `FilePos` from `f`, truncated toward zero.
	**/
	public static inline function fromFloat(f:Float):FilePos {
		#if (hl || jvm || cpp || eval)
		return new FilePos(haxe.Int64.fromFloat(f));
		#else
		return new FilePos(f < 0 ? Math.fceil(f) : Math.ffloor(f));
		#end
	}

	/**
		Converts to `Int`, throwing "Overflow" when the value is outside the
		32-bit `Int` range.
	**/
	public inline function toInt():Int {
		#if (hl || jvm || cpp || eval)
		return haxe.Int64.toInt(this);
		#else
		if (this < -2147483648. || this > 2147483647.)
			throw "Overflow";
		return Std.int(this);
		#end
	}

	public inline function toFloat():Float {
		#if (hl || jvm || cpp || eval)
		return this.toFloat();
		#else
		return this;
		#end
	}

	public inline function toInt64():haxe.Int64 {
		#if (hl || jvm || cpp || eval)
		return this;
		#else
		return haxe.Int64.fromFloat(this);
		#end
	}

	public inline function toString():String {
		#if (hl || jvm || cpp || eval)
		return haxe.Int64.toStr(this);
		#else
		return Std.string(this);
		#end
	}

	inline function data():FilePosData
		return this;

	@:op(-A) static inline function neg(a:FilePos):FilePos
		return new FilePos(-a.data());

	@:op(A + B) static inline function add(a:FilePos, b:FilePos):FilePos
		return new FilePos(a.data() + b.data());

	@:op(A - B) static inline function sub(a:FilePos, b:FilePos):FilePos
		return new FilePos(a.data() - b.data());

	@:op(A * B) @:commutative static inline function mul(a:FilePos, b:Int):FilePos
		return new FilePos(a.data() * b);

	// like Int, division gives a Float
	@:op(A / B) static inline function div(a:FilePos, b:Float):Float
		return a.toFloat() / b;

	@:op(A / B) static inline function divPos(a:FilePos, b:FilePos):Float
		return a.toFloat() / b.toFloat();

	@:op(A % B) static inline function mod(a:FilePos, b:Int):Int {
		#if (hl || jvm || cpp || eval)
		return haxe.Int64.toInt(a.data() % b);
		#else
		return Std.int(a.data() % b);
		#end
	}

	@:op(A == B) static inline function eq(a:FilePos, b:FilePos):Bool
		return a.data() == b.data();

	@:op(A != B) static inline function neq(a:FilePos, b:FilePos):Bool
		return a.data() != b.data();

	@:op(A < B) static inline function lt(a:FilePos, b:FilePos):Bool
		return a.data() < b.data();

	@:op(A <= B) static inline function lte(a:FilePos, b:FilePos):Bool
		return a.data() <= b.data();

	@:op(A > B) static inline function gt(a:FilePos, b:FilePos):Bool
		return a.data() > b.data();

	@:op(A >= B) static inline function gte(a:FilePos, b:FilePos):Bool
		return a.data() >= b.data();
}
