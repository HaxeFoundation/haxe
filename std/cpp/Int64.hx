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

package cpp;

@:coreType @:notNull @:runtimeValue abstract Int64 from Int {

	/**
		Destructively cast to Int
	**/
	public inline function toInt():Int {
		return cast this;
	}

	@:to
	@:deprecated("Implicit cast from Int64 to Int (32 bits) is deprecated. Use .toInt() or explicitly cast instead.")
	inline function implicitToInt(): Int {
		return toInt();
	}

	@:to
	#if !cppia inline #end function toInt64():haxe.Int64 {
		return cast this;
	}

	@:from
	static #if !cppia inline #end function ofInt64(x:haxe.Int64):Int64 {
		return cast x;
	}
}
