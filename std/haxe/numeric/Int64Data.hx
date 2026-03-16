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
	Internal backing class for the emulated `Int64Native` abstract.
	Not intended for direct use — use `haxe.Int64` instead.
**/
@:noCompletion
class Int64Data {
	public var high:haxe.Int32;
	public var low:haxe.Int32;

	public inline function new(high:haxe.Int32, low:haxe.Int32) {
		this.high = high;
		this.low = low;
	}

	@:ifFeature("dynamic_read.toString")
	public function toString():String {
		if (high == 0 && low == 0)
			return "0";
		var negative = high < 0;
		// Split into four unsigned 16-bit chunks for safe division
		var h:Int, l:Int;
		if (negative) {
			h = ~high;
			l = -low;
			if (l == 0)
				h++;
		} else {
			h = high;
			l = low;
		}
		var d3 = (h >>> 16) & 0xFFFF;
		var d2 = h & 0xFFFF;
		var d1 = (l >>> 16) & 0xFFFF;
		var d0 = l & 0xFFFF;
		var str = "";
		while (d3 != 0 || d2 != 0 || d1 != 0 || d0 != 0) {
			// Divide the 4-chunk number by 10, propagating remainders
			var r = d3 % 10;
			d3 = Std.int(d3 / 10);
			var v = r * 65536 + d2;
			d2 = Std.int(v / 10);
			r = v % 10;
			v = r * 65536 + d1;
			d1 = Std.int(v / 10);
			r = v % 10;
			v = r * 65536 + d0;
			d0 = Std.int(v / 10);
			str = (v % 10) + str;
		}
		if (negative)
			str = "-" + str;
		return str;
	}
}
