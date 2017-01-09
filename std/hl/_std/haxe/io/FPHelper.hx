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
package haxe.io;

class FPHelper {

	// note : this is not thread safe, use TLS when available
	static var i64tmp = Int64.ofInt(0);
	static var helper = new hl.Bytes(8);

	public static function i32ToFloat( i : Int ) : Single {
		helper.setI32(0,i);
		return helper.getF32(0);
	}

	public static function floatToI32( f : Single ) : Int {
		helper.setF32(0,f);
		return helper.getI32(0);
	}

	public static function i64ToDouble( low : Int, high : Int ) : Float {
		helper.setI32(0,low);
		helper.setI32(4,high);
		return helper.getF64(0);
	}

	public static function doubleToI64( v : Float ) : Int64 {
		helper.setF64(0,v);
		var i64 = i64tmp;
		@:privateAccess {
			i64.set_low(helper.getI32(0));
			i64.set_high(helper.getI32(4));
		}
		return i64;
	}

}
