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

/*
 * This File is partly based on the following files of The LLVM Compiler Infrastructure.
 * It's ported to Haxe and modified to fit the needs of this Library, the original Licence can be found in
 * LICENSE-ORIGINAL.TXT.
 *
 * http://llvm.org/svn/llvm-project/llvm/trunk/include/llvm/Support/ConvertUTF.h
 * http://llvm.org/svn/llvm-project/llvm/trunk/lib/Support/ConvertUTF.cpp
 *
 */
package haxe.i18n;
import haxe.i18n.ByteAccess;
import haxe.i18n.ByteAccessBuffer;
import haxe.ds.Vector;
import haxe.i18n.Ucs2.Ucs2Reader;
import haxe.i18n.Utf8.Utf8Reader;
import haxe.i18n.Utf16.Utf16Reader;
import haxe.i18n.Utf32.Utf32Reader;
import haxe.io.BytesData;

enum ConversionError {
	SourceConsumed;
	SourceIllegal(bytePos:Int);
}

@:allow(haxe.i18n)
class Convert {
	static inline var HALF_SHIFT:Int  = 10; /* used for shifting by 10 bits */

	static inline var HALF_BASE:Int = 0x0010000;
	static inline var HALF_MASK:Int = 0x3FF;

	static inline var UNI_SUR_HIGH_START:Int = 0xD800;
	static inline var UNI_SUR_HIGH_END:Int = 0xDBFF;

	static inline var UNI_SUR_LOW_START:Int = 0xDC00;
	static inline var UNI_SUR_LOW_END:Int = 0xDFFF;

	static inline var UNI_REPLACEMENT_CHAR:Int = 0x0000FFFD;
	static inline var UNI_MAX_BMP:Int = 0x0000FFFF;
	static inline var UNI_MAX_UTF16:Int = 0x0010FFFF;

	static inline var UNI_MAX_LEGAL_UTF32:Int = 0x0010FFFF;

	/*
	 * Index into the table below with the first byte of a UTF-8 sequence to
	 * get the number of trailing bytes that are supposed to follow it.
	 * Note that *legal* UTF-8 values can't have 4 or 5-bytes. The table is
	 * left as-is for anyone who may want to do such conversion, which was
	 * allowed in earlier algorithms.
	 */
	static var trailingBytesForUtf8:Vector<Int> = Vector.fromArrayCopy([
		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
		1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
		2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, 3,3,3,3,3,3,3,3,4,4,4,4,5,5,5,5
	]);

	/*
	 * Magic values subtracted from a buffer value during UTF8 conversion.
	 * This table contains as many values as there might be trailing bytes
	 * in a UTF-8 sequence.
	 */
	static var offsetsFromUtf8:Vector<Int> = Vector.fromArrayCopy([
		0x00000000, 0x00003080, 0x000E2080,
		0x03C82080, 0xFA082080, 0x82082080
	]);

	/*
	 * Once the bits are split out into bytes of UTF-8, this is a mask OR-ed
	 * into the first byte, depending on how many bytes follow.  There are
	 * as many entries in this table as there are UTF-8 sequence types.
	 * (I.e., one byte sequence, two byte... etc.). Remember that sequencs
	 * for *legal* UTF-8 will be 4 or fewer bytes total.
	 */
	static var firstByteMark:Vector<Int> =  Vector.fromArrayCopy([
		0x00, 0x00, 0xC0, 0xE0, 0xF0, 0xF8, 0xFC
	]);

	public static inline function isHighSurrogate(codeUnitHigh : Int) : Bool {
		return UNI_SUR_HIGH_START <= codeUnitHigh && codeUnitHigh <= UNI_SUR_HIGH_END;
	}

	static inline function isLowSurrogate(codeUnitLow : Int) : Bool {
		return UNI_SUR_LOW_START <= codeUnitLow && codeUnitLow <= UNI_SUR_LOW_END;
	}

	static inline function isSurrogate(codeUnit : Int) : Bool {
		return codeUnit >= UNI_SUR_HIGH_START && codeUnit <= UNI_SUR_LOW_END;
	}

	public static inline function isSurrogatePair(charCode : Int) : Bool {
		return charCode > UNI_MAX_BMP && charCode <= UNI_MAX_UTF16;
	}

	public static inline function surrogatePairToCharCode (codeUnitHigh:Int, codeUnitLow:Int) {
		return ((codeUnitHigh - UNI_SUR_HIGH_START) << HALF_SHIFT)
			+ (codeUnitLow - UNI_SUR_LOW_START) + HALF_BASE;
	}

	static inline function codePointToSurrogatePair (codePoint:Int) {
		codePoint -= HALF_BASE;
		var high = (codePoint >> HALF_SHIFT) + UNI_SUR_HIGH_START;
		var low = (codePoint & HALF_MASK) + UNI_SUR_LOW_START;
		return { high : high, low : low };
	}

	static inline function writeUtf8CodeUnits (charCode:Int, bytesToWrite:Int, addByte:Int->Void) {
		var byteMask:Int = 0xBF;
		var byteMark:Int = 0x80;

		switch (bytesToWrite)
		{
			case 4:
				addByte( (   (charCode >> 18) | firstByteMark[bytesToWrite]) );
				addByte( ( ( (charCode >> 12) | byteMark) & byteMask) );
				addByte( ( ( (charCode >> 6 ) | byteMark) & byteMask) );
				addByte( ( ( (charCode      ) | byteMark) & byteMask) );
			case 3:
				addByte( (   (charCode >> 12) | firstByteMark[bytesToWrite]) );
				addByte( ( ( (charCode >> 6 ) | byteMark) & byteMask) );
				addByte( ( ( (charCode      ) | byteMark) & byteMask) );
			case 2:
				addByte( (   (charCode >> 6 ) | firstByteMark[bytesToWrite]) );
				addByte( ( ( (charCode      ) | byteMark) & byteMask) );
			case 1:
				addByte( (   (charCode      ) | firstByteMark[bytesToWrite]) );
		}
	}

	static inline function getUtf8SequenceSizeFromCodePoint (codePoint:Int) {
		if (codePoint <= 0x7F) {
			return 1;
		} else if (codePoint <= 0x7FF) {
			return 2;
		} else if (codePoint <= UNI_MAX_BMP) {
			return 3;
		} else if (codePoint <= UNI_MAX_LEGAL_UTF32) {
			return 4;
		} else {
			return 3;
		}
	}

	static inline function isInvalidUtf8CodePoint (codePoint:Int) {
		return codePoint > UNI_MAX_LEGAL_UTF32;

	}

	public static inline function getUtf8SequenceSize (firstCodeUnit:Int) {
		return trailingBytesForUtf8[firstCodeUnit]+1;
	}

	// asserts valid utf8 bytes
	public static inline function charCodeFromUtf8Bytes (source:Utf8Reader, pos:Int, size:Int) {
		return charCodeFromUtf8( pos -> source.fastGet(pos), pos, size);
	}

	public static inline function getUtf16CodeSize (codePoint:Int):Int {
		return if (codePoint <= UNI_MAX_BMP) 2 else 4;
	}

	public static inline function charCodeFromUtf8 (codeUnitAt:Int->Int, pos:Int, size:Int) {
		var trailingBytes = size - 1;
		var code = 0;
		switch (trailingBytes) {
			case 5:
				code += codeUnitAt(pos++); code <<= 6;
				code += codeUnitAt(pos++); code <<= 6;
				code += codeUnitAt(pos++); code <<= 6;
				code += codeUnitAt(pos++); code <<= 6;
				code += codeUnitAt(pos++); code <<= 6;
				code += codeUnitAt(pos++);
			case 4:
				code += codeUnitAt(pos++); code <<= 6;
				code += codeUnitAt(pos++); code <<= 6;
				code += codeUnitAt(pos++); code <<= 6;
				code += codeUnitAt(pos++); code <<= 6;
				code += codeUnitAt(pos++);
			case 3:
				code += codeUnitAt(pos++); code <<= 6;
				code += codeUnitAt(pos++); code <<= 6;
				code += codeUnitAt(pos++); code <<= 6;
				code += codeUnitAt(pos++);
			case 2:
				code += codeUnitAt(pos++); code <<= 6;
				code += codeUnitAt(pos++); code <<= 6;
				code += codeUnitAt(pos++);
			case 1:
				code += codeUnitAt(pos++); code <<= 6;
				code += codeUnitAt(pos++);
			case 0:
				code += codeUnitAt(pos++);
			case _:
				throw "assert";
		}
		code -= offsetsFromUtf8[trailingBytes];
		return code;
	}

	public static function isLegalUtf8Source(source:Utf8Reader):Bool {
		var i = 0;
		while (i < source.length) {
			var size = getUtf8SequenceSize(source.fastGet(i));
			if (i + size > source.length || !isLegalUtf8Sequence(source, i, size))
				return false;
			i += size;
		}
		return true;
	}
	/*
	 * Utility routine to tell whether a sequence of bytes is legal UTF-8.
	 * This must be called with the length pre-determined by the first byte.
	 * If not calling this from ConvertUTF8to*, then the length can be set by:
	 *  length = trailingBytesForUtf8[*source]+1;
	 * and the sequence is illegal right away if there aren't that many bytes
	 * available.
	 * If presented with a length > 4, this returns false.  The Unicode
	 * definition of UTF-8 goes up to 4-byte sequences.
	 */

	static function isLegalUtf8Sequence( source:Utf8Reader, pos:Int, length:Int):Bool
	{
		var unit0 = source.fastGet(pos);

		inline function check4 () {
			var unit3 = source.fastGet(pos + 3);
			return if (unit3 < 0x80 || unit3 > 0xBF) false else true;
		}

		inline function check3 () {
			var unit2 = source.fastGet(pos + 2);
			return if (unit2 < 0x80 || unit2 > 0xBF) false else true;
		}

		inline function check2 () {
			var unit1 = source.fastGet(pos + 1);
			return if (unit1 < 0x80 || unit1 > 0xBF) {
				false;
			} else switch (unit0) {
				case 0xE0 if (unit1 < 0xA0): false;
				case 0xED if (unit1 > 0x9F): false;
				case 0xF0 if (unit1 < 0x90): false;
				case 0xF4 if (unit1 > 0x8F): false;
				case _    if (unit1 < 0x80): false;
				case _: true;
			}
		}

		inline function check1 () {
			return if (unit0 >= 0x80 && unit0 < 0xC2) false else true;
		}

		inline function check0 () {
			return unit0 <= 0xF4;
		}

		return switch (length)
		{
			case 4:
				check4() && check3() && check2() && check1() && check0();
			case 3:
				check3() && check2() && check1() && check0();
			case 2:
				check2() && check1() && check0();
			case 1:
				check1() && check0();
			case _:
				false;
		}
	}

	static function findMaximalSubpartOfIllFormedUtf8Sequence(source:Utf8Reader, pos:Int) {
		var sourceEnd = source.length;

		/*
		* Unicode 6.3.0, D93b:
		*
		*   Maximal subpart of an ill-formed subsequence: The longest code unit
		*   subsequence starting at an unconvertible offset that is either:
		*   a. the initial subsequence of a well-formed code unit sequence, or
		*   b. a subsequence of length one.
		*/

		if (pos == sourceEnd)
			return 0;

		/*
		* Perform case analysis.  See Unicode 6.3.0, Table 3-7. Well-Formed UTF-8
		* Byte Sequences.
		*/

		var b1 = source.fastGet(pos);
		++pos;

		if (b1 >= 0xC2 && b1 <= 0xDF) {
			/*
			* First byte is valid, but we know that this code unit sequence is
			* invalid, so the maximal subpart has to end after the first byte.
			*/
			return 1;
		}

		if (pos == sourceEnd)
			return 1;

		var b2 = source.fastGet(pos);
		++pos;

		if (b1 == 0xE0) {
			return (b2 >= 0xA0 && b2 <= 0xBF) ? 2 : 1;
		}
		if (b1 >= 0xE1 && b1 <= 0xEC) {
			return (b2 >= 0x80 && b2 <= 0xBF) ? 2 : 1;
		}
		if (b1 == 0xED) {
			return (b2 >= 0x80 && b2 <= 0x9F) ? 2 : 1;
		}
		if (b1 >= 0xEE && b1 <= 0xEF) {
			return (b2 >= 0x80 && b2 <= 0xBF) ? 2 : 1;
		}
		if (b1 == 0xF0) {
			if (b2 >= 0x90 && b2 <= 0xBF) {
			if (pos == sourceEnd)
				return 2;

			var b3 = source.fastGet(pos);
			return (b3 >= 0x80 && b3 <= 0xBF) ? 3 : 2;
			}
			return 1;
		}
		if (b1 >= 0xF1 && b1 <= 0xF3) {
			if (b2 >= 0x80 && b2 <= 0xBF) {
			if (pos == sourceEnd)
				return 2;

			var b3 = source.fastGet(pos);
			return (b3 >= 0x80 && b3 <= 0xBF) ? 3 : 2;
			}
			return 1;
		}
		if (b1 == 0xF4) {
			if (b2 >= 0x80 && b2 <= 0x8F) {
			if (pos == sourceEnd)
				return 2;

			var b3 = source.fastGet(pos);
			return (b3 >= 0x80 && b3 <= 0xBF) ? 3 : 2;
			}
			return 1;
		}

		if ((b1 >= 0x80 && b1 <= 0xC1) || b1 >= 0xF5) throw "assert";
		/*
		* There are no valid sequences that start with these bytes.  Maximal subpart
		* is defined to have length 1 in these cases.
		*/
		return 1;
	}

	 public inline static function writeUtf16CodeUnits(codePoint:Int, pos:Int, addInt16:Int->Int->Void, strict:Bool, strictUcs2:Bool) {
		if (codePoint <= UNI_MAX_BMP) {
			/* UTF-16 surrogate values are illegal in UTF-32 */
			if (isSurrogate(codePoint)) {
				if (strict) {
					throw SourceIllegal(pos);
				} else {
					addInt16(pos, UNI_REPLACEMENT_CHAR);
				}
			} else {
				addInt16(pos, codePoint);
			}
		} else if (codePoint > UNI_MAX_UTF16) {
			if (strict) {
				throw SourceIllegal(pos);
			} else {
				addInt16(pos, UNI_REPLACEMENT_CHAR);
			}
		} else {
			if (strictUcs2) {
				throw SourceIllegal(pos);
			} else {
				var units = codePointToSurrogatePair(codePoint);
				addInt16(pos, units.high);
				addInt16(pos+1, units.low);
			}
		}
	}

	static function convertUtf8toUtf16Impl (source:Utf8Reader, strict:Bool, strictUcs2:Bool):ByteAccess {
		var target = new ByteAccessBuffer();
		var i = 0;
		while (i < source.length) {
			var size = getUtf8SequenceSize(source.fastGet(i));
			if (size > source.length - i) {
				throw SourceConsumed;
			}
			/* Do this check in every case */
			if (!isLegalUtf8Sequence(source, i, size)) {
				throw SourceIllegal(i);
			}
			var charCode = charCodeFromUtf8Bytes(source, i, size);

			writeUtf16CodeUnits(charCode, i, function (_, unit) target.addInt16BigEndian(unit), strict, strictUcs2 );
			i+=size;
		}
		return target.getByteAccess();
	}

	/* code to byteAccess utf8, utf16, utf32 */

	public static function codePointToUtf8ByteAccess (codePoint:Int):ByteAccess {
		var size = getUtf8SequenceSizeFromCodePoint(codePoint);
		var bytes = ByteAccess.alloc(size);
		var pos = 0;
		writeUtf8CodeUnits(codePoint, size, function (x) {
			bytes.set(pos, x);
			pos++;
		});
		return bytes;
	}

	public static function codePointToUtf16ByteAccess (charCode:Int):ByteAccess {
		var size = getUtf16CodeSize(charCode);
		var bytes = ByteAccess.alloc(size);

		Convert.writeUtf16CodeUnits(charCode, 0,
			function (pos, val) {
				bytes.set(pos << 1, (val >> 8) & 0xFF );
				bytes.set((pos << 1)+1, val & 0xFF);
			}, true, false);
		return bytes;
	}

	/* Conversions from utf8 */

	public static inline function convertUtf8toUtf16 (source:Utf8Reader, strict:Bool):ByteAccess {
		return convertUtf8toUtf16Impl(source, strict, false);
	}

	public static inline function convertUtf8toUcs2 (source:Utf8Reader, strict:Bool, strictUcs2:Bool):ByteAccess {
		return convertUtf8toUtf16Impl(source, strict, strictUcs2);
	}

	public static function convertUtf8toUtf32(source:Utf8Reader, strict:Bool):ByteAccess {
		var i = 0;
		var target = new ByteAccessBuffer();
		while (i < source.length) {
			var size = getUtf8SequenceSize(source.fastGet(i));
			if (size > source.length - i) {
				if (strict) {
					throw SourceConsumed;
				} else {
					/*
					* Replace the maximal subpart of ill-formed sequence with
					* replacement character.
					*/
					i += findMaximalSubpartOfIllFormedUtf8Sequence(source, i);
					target.addInt32BigEndian(UNI_REPLACEMENT_CHAR);
					continue;
				}
			}
			/* Do this check whether lenient or strict */
			if (!isLegalUtf8Sequence(source, i, size)) {
				if (strict) {
					throw SourceIllegal(i);
				} else {
					/*
					* Replace the maximal subpart of ill-formed sequence with
					* replacement character.
					*/
					i += findMaximalSubpartOfIllFormedUtf8Sequence(source, i);
					target.addInt32BigEndian(UNI_REPLACEMENT_CHAR);
					continue;
				}
			}

			var code = charCodeFromUtf8Bytes(source, i, size);

			if (code <= UNI_MAX_LEGAL_UTF32) {
				/*
				* UTF-16 surrogate values are illegal in UTF-32, and anything
				* over Plane 17 (> 0x10FFFF) is illegal.
				*/
				if (isSurrogate(code)) {
					if (strict) {
						throw SourceIllegal(i);
					} else {
						target.addInt32BigEndian(UNI_REPLACEMENT_CHAR);
					}
				} else {
					target.addInt32BigEndian(code);
				}
			} else { /* i.e., ch > UNI_MAX_LEGAL_UTF32 */
				throw SourceIllegal(i);
			}
			i+=size;
		}

		return target.getByteAccess();
	}

	/* Conversions from utf16 */

	public static inline function convertUtf16toUtf8 (source:Utf16Reader, strict:Bool):ByteAccess {
		var target = new ByteAccessBuffer();
		var i = 0;
		if (source.length % 2 == 1) throw "invalid source length " + source.length;
		while (i < source.length) {
			var unit = source.getInt16(i);
			var code = unit;
			var pos = i; // store position for error reporting
			i+=2;
			/* If we have a surrogate pair, convert to UTF32 first. */
			if (isHighSurrogate(unit)) {
				if (i < source.length) { /* If the 16 bits following the high surrogate are in the source buffer... */
					var unit2 = source.getInt16(i);
					/* If it's a low surrogate, convert to UTF32. */
					if (isLowSurrogate(unit2)) {
						code = surrogatePairToCharCode(unit, unit2);
						i+=2;
					} else if (strict) { /* it's an unpaired high surrogate */
						/*  return to the illegal value itself */
						throw SourceIllegal(pos);
					}
				} else { /* We don't have the 16 bits following the high surrogate. */
					throw SourceConsumed;
				}
			} else if (strict) {
				/* UTF-16 surrogate values are illegal in UTF-32 */
				if (isLowSurrogate(unit)) {
					throw SourceIllegal(pos);
				}
			}

			var size = getUtf8SequenceSizeFromCodePoint(code);
			if (isInvalidUtf8CodePoint(code)) {
				if (strict) {
					throw SourceIllegal(pos);
				} else {
					code = UNI_REPLACEMENT_CHAR;
				}
			}
			writeUtf8CodeUnits(code, size, function (x) target.addByte(x));
		}

		return target.getByteAccess();
	}

	public static function convertUtf16toUtf32 (source:Utf16Reader, strict:Bool) {
		var i = 0;
		var target = new ByteAccessBuffer();

		while (i < source.length)
		{
			var unit = source.getInt16(i);

			/* If we have a surrogate pair, convert to UTF32 first. */
			if (isHighSurrogate(unit)) {
				/* If the 16 bits following the high surrogate are in the source buffer... */
				if (i < source.length - 2) {
					var unit2 = source.getInt16(i+2);
					/* If it's a low surrogate, convert to UTF32. */
					if (isLowSurrogate(unit2)) {
						unit = surrogatePairToCharCode(unit, unit2);
						i+=2;
					} else if (strict) { /* it's an unpaired high surrogate */
						throw SourceIllegal(i);
					}
				} else { /* We don't have the 16 bits following the high surrogate. */
					throw SourceConsumed;
				}
			} else if (strict) {
				/* UTF-16 surrogate values are illegal in UTF-32 */
				if (isLowSurrogate(unit)) {
					throw SourceIllegal(i);
				}
			}
			target.addInt32BigEndian(unit);
			i += 2;
		}
		return target.getByteAccess();
	}

	/* Conversions from Utf32 */

	public static function convertUtf32toUtf8 (source:Utf32Reader, strict:Bool):ByteAccess {
		var i = 0;
		var target = new ByteAccessBuffer();

		while (i < source.length) {
			var code = source.getInt32(i);

			if (strict && isSurrogate(code) ) {
				/* UTF-16 surrogate values are illegal in UTF-32 */
				throw SourceIllegal(i);
			}
			var size = getUtf8SequenceSizeFromCodePoint(code);

			if (isInvalidUtf8CodePoint(code)) {
				if (strict) {
					throw SourceIllegal(i);
				} else {
					code = UNI_REPLACEMENT_CHAR;
				}
			}
			writeUtf8CodeUnits(code, size, function (x) target.addByte(x));
			i+=4;
		}
		return target.getByteAccess();
	}

	public static function convertUtf32toUtf16 (source:Utf32Reader, strict:Bool) {
		var i = 0;
		var target = new ByteAccessBuffer();

		while (i < source.length) {
			var unit = source.getInt32(i);

			if (unit <= UNI_MAX_BMP) { /* Target is a character <= 0xFFFF */
				/* UTF-16 surrogate values are illegal in UTF-32; 0xffff or 0xfffe are both reserved values */
				if (isSurrogate(unit)) {
					if (strict) {
						throw SourceIllegal(i);
					} else {
						target.addInt16BigEndian(UNI_REPLACEMENT_CHAR);
					}
				} else {
					target.addInt16BigEndian(unit);
				}
			} else if (unit > UNI_MAX_LEGAL_UTF32) {
				if (strict) {
					throw SourceIllegal(i);
				} else {
					target.addInt16BigEndian(UNI_REPLACEMENT_CHAR);
				}
			} else {
				var pair = codePointToSurrogatePair(unit);

				target.addInt16BigEndian(pair.high);
				target.addInt16BigEndian(pair.low);
			}
			i+=4;
		}
		return target.getByteAccess();
	}
}

class NativeStringTools {

	/* iterate character codes from native strings */
	static inline function eachCharCode (s:String, f:Int->Int->Void) {
		#if (eval || neko || cpp || php || lua)
		// native=utf8, iterate codes
		var i = 0;
		while( i < s.length ) {
			var size = Convert.getUtf8SequenceSize(StringTools.fastCodeAt(s, i));
			var code = Convert.charCodeFromUtf8( pos -> StringTools.fastCodeAt(s, pos), i, size);
			f(code, i);
			i+=size;
		}
		#elseif (js || java || cs || flash || as3 || hl)
		// native=utf16, iterate codes
		var i = 0;
		while( i < s.length ) {
			var pos = i;
			var c : Int = StringTools.fastCodeAt(s,i++);
			// surrogate pair
			if( 0xD800 <= c && c <= 0xDBFF )
				c = (c - 0xD7C0 << 10) | (StringTools.fastCodeAt(s,i++) & 0x3FF);
			f(c, pos);
		}
		#elseif python
		// native=utf32, iterate codes
		var i = 0;
		while( i < s.length ) {
			var c : Int = StringTools.fastCodeAt(s,i);
			f(c, i);
			i++;
		}
		#end
	}

	public static function toUtf16ByteAccess (s:String):ByteAccess {
		#if python
		return ByteAccess.ofData(python.NativeStringTools.encode(s, "utf-16be"));
		#elseif java
		try
		{
			var b:BytesData = untyped s.getBytes("UTF-16BE");
			return ByteAccess.ofData(b);
		}
		catch (e:Dynamic) throw e;
		#elseif cs
		var b = cs.system.text.Encoding.BigEndianUnicode.GetBytes(s);
		return ByteAccess.ofData(b);
		#else
		var buf = new ByteAccessBuffer();
		var pos = 0;
		eachCharCode(s, (code, _) -> {
			Tools.Convert.writeUtf16CodeUnits(code, pos, (_, int16) -> {
				pos++;
				buf.addInt16BigEndian(int16);
			}, true, false);
		});
		return buf.getByteAccess();
		#end
	}

	public static inline function toUcs2ByteAccess (s:String):ByteAccess {
		return toUtf16ByteAccess(s);
	}

	public static function toUtf32Vector (s:String):Vector<Int>
	{
		var v = new Vector<Int>(s.length);
		var index = 0;
		eachCharCode(s, (code, _) -> {
			v[index++] = code;
		});

		// early exit if vector.length is the same as string.length
		if (index == s.length) {
			return v;
		} else {
			// our vector is larger than required
			// because index (number of utf32 characters) < s.length
			var res = new Vector<Int>(index);
			Vector.blit(v, 0, res, 0, index);
			return res;
		}
	}

	/*
	public static function toUtf32ByteAccess (s:String):ByteAccess {
		#if python
		return ByteAccess.ofData(python.NativeStringTools.encode(s, "utf-32be"));
		#elseif java
		try
		{
			var b:BytesData = untyped s.getBytes("UTF-32BE");
			return ByteAccess.ofData(b);
		}
		catch (e:Dynamic) throw e;
		#elseif cs
		var b = cs.system.text.Encoding.UTF32.GetBytes(s);
		return ByteAccess.ofData(b);
		#else
		return Convert.convertUtf8toUtf32(new Utf8Reader(toUtf8(s)), true);
		#end
	}
	*/


	public static function toUtf8ByteAccess (s:String):ByteAccess {
		#if neko
		return ByteAccess.ofData(untyped __dollar__ssub(s.__s,0,s.length));
		#elseif flash
		var b = new flash.utils.ByteArray();
		b.writeUTFBytes(s);
		return ByteAccess.ofData(b);
		#elseif php
		var x = #if php7 (s:BytesData) #else BytesData.ofString(s) #end;
		return ByteAccess.ofData(x);
		#elseif cpp
		var a = new BytesData();
		untyped __global__.__hxcpp_bytes_of_string(a,s);
		return ByteAccess.ofData(a);
		#elseif cs
		var b = cs.system.text.Encoding.UTF8.GetBytes(s);
		return ByteAccess.ofData(b);
		#elseif java
		try
		{
			var b:BytesData = untyped s.getBytes("UTF-8");
			return ByteAccess.ofData(b);
		}
		catch (e:Dynamic) throw e;
		#elseif python
		return ByteAccess.ofData(python.NativeStringTools.encode(s, "utf-8"));
		#elseif lua
		var bytes = [for (c in 0...s.length) StringTools.fastCodeAt(s,c)];
		return ByteAccess.ofData(bytes);
		#elseif hl
		var size = 0;
		var b = @:privateAccess s.bytes.utf16ToUtf8(0, size);
		return ByteAccess.ofData(new BytesData(b,size));
		#elseif eval
		return ByteAccess.ofData(haxe.io.Bytes.ofString(s));
		#else
		var a = new Array();
		eachCharCode(s, (c, pos) -> {
			if (Convert.isInvalidUtf8CodePoint(c)) {
				throw SourceIllegal(pos);
			}
			var size = Convert.getUtf8SequenceSizeFromCodePoint(c);
			Convert.writeUtf8CodeUnits(c, size, function (x) a.push(x));
		});
		#if js
		return ByteAccess.ofData(new js.html.Uint8Array(a).buffer);
		#else
		return ByteAccess.ofData(a);
		#end
		#end
	}
}