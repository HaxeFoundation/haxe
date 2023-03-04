/*
 * Copyright (C)2005-2023 Haxe Foundation
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

package haxe.math.bigint;

import haxe.math.bigint.BigIntException;
import haxe.math.bigint.BigIntError;

/* Original code courtesy Chuck Batson (github.com/cbatson) */
class BigIntTools {
	public static inline function isNull(value:BigInt):Bool {
		var a:BigInt_ = value;
		return a == null;
	}

	public static inline function isBigInt(value:Dynamic):Bool {
		return Std.isOfType(value, BigInt_);
	}

	public static inline function castFrom(value:Dynamic):BigInt {
		return new BigInt(Std.downcast(value, BigInt_));
	}

	public static function parseValueUnsigned(value:Dynamic):BigInt {
		var bi:BigInt;
		if (Std.isOfType(value, String)) {
			bi = parseStringUnsigned(cast(value, String));
		} else if (isBigInt(value)) {
			var t = new MutableBigInt_();
			t.copyFrom(castFrom(value));
			return new BigInt(t);
		} else if (Std.isOfType(value, Int)) {
			bi = BigInt.fromInt(cast(value, Int));
		} else {
			throw new BigIntException(BigIntError.INVALID_ARGUMENT);
		}
		return bi;
	}

	private static function parseStringUnsigned(value:String):BigInt {
		var result = new MutableBigInt_();
		if (StringTools.startsWith(value, "0x")) {
			result.setFromHexUnsigned(value.substr(2));
		} else {
			result.setFromString(value);
		}
		var result2:MutableBigInt = result;
		return result2;
	}
}
