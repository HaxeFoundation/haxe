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

package haxe.numeric;

/**
	Cross-platform emulation of a 64-bit integer using two 32-bit words.
	This is the default implementation used on targets without native 64-bit
	integer support. Targets with native support can shadow this file via
	their `_std` directory to provide an optimized version.
**/
class Int64Native {
	public var high:haxe.Int32;
	public var low:haxe.Int32;

	public inline function new(high:haxe.Int32, low:haxe.Int32) {
		this.high = high;
		this.low = low;
	}

	/**
		We also define toString here to ensure we always get a pretty string
		when tracing or calling `Std.string`. This tends not to happen when
		`toString` is only in the abstract.
	**/
	@:ifFeature("dynamic_read.toString")
	public function toString():String
		return haxe.Int64.toStr(cast this);
}
