package haxe.i18n;
import haxe.i18n.ByteAccess;
import haxe.i18n.ByteAccessBuffer;



/***** SOURCE
// http://llvm.org/svn/llvm-project/llvm/trunk/include/llvm/Support/ConvertUTF.h
// http://llvm.org/svn/llvm-project/llvm/trunk/lib/Support/ConvertUTF.cpp

*****/

/*===--- ConvertUTF.c - Universal Character Names conversions ---------------===
 *
 *                     The LLVM Compiler Infrastructure
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See LICENSE.TXT for details.
 *
 *===------------------------------------------------------------------------=*/
/*
 * Copyright 2001-2004 Unicode, Inc.
 *
 * Disclaimer
 *
 * This source code is provided as is by Unicode, Inc. No claims are
 * made as to fitness for any particular purpose. No warranties of any
 * kind are expressed or implied. The recipient agrees to determine
 * applicability of information provided. If this file has been
 * purchased on magnetic or optical media from Unicode, Inc., the
 * sole remedy for any claim will be exchange of defective media
 * within 90 days of receipt.
 *
 * Limitations on Rights to Redistribute This Code
 *
 * Unicode, Inc. hereby grants the right to freely use the information
 * supplied in this file in the creation of products supporting the
 * Unicode Standard, and to make copies of this file in any form
 * for internal or external distribution as long as this notice
 * remains attached.
 */

/* ---------------------------------------------------------------------

    Conversions between UTF32, UTF-16, and UTF-8. Source code file.
    Author: Mark E. Davis, 1994.
    Rev History: Rick McGowan, fixes & updates May 2001.
    Sept 2001: fixed const & error conditions per
        mods suggested by S. Parent & A. Lillich.
    June 2002: Tim Dodd added detection and handling of incomplete
        source sequences, enhanced error detection, added casts
        to eliminate compiler warnings.
    July 2003: slight mods to back out aggressive FFFE detection.
    Jan 2004: updated switches in from-UTF8 conversions.
    Oct 2004: updated to use UNI_MAX_LEGAL_UTF32 in UTF-32 conversions.

    See the header file "ConvertUTF.h" for complete documentation.

------------------------------------------------------------------------ */


//#include "llvm/Support/ConvertUTF.h"
//#ifdef CVTUTF_DEBUG
//#include <stdio.h>
//#endif
//#include <assert.h>

import haxe.ds.Vector;


enum ConversionError {
    SourceExhausted;
    SourceIllegal(bytePos:Int);
}

@:enum abstract ConversionFlags(Int) {
  var StrictConversion = 0;
  var LenientConversion = 1;
}


class Encoding {
    static inline var halfShift:Int  = 10; /* used for shifting by 10 bits */

    static inline var halfBase:Int = 0x0010000;
    static inline var halfMask:Int = 0x3FF;

    static inline var UNI_SUR_HIGH_START:Int = 0xD800;
    static inline var UNI_SUR_HIGH_END:Int = 0xDBFF;

    static inline var UNI_SUR_LOW_START:Int = 0xDC00;
    static inline var UNI_SUR_LOW_END:Int = 0xDFFF;

    static inline var UNI_REPLACEMENT_CHAR:Int = 0x0000FFFD;
    static inline var UNI_MAX_BMP:Int = 0x0000FFFF;
    static inline var UNI_MAX_UTF16:Int = 0x0010FFFF;
    static inline var UNI_MAX_UTF32:Int = 0x7FFFFFFF;
    static inline var UNI_MAX_LEGAL_UTF32:Int = 0x0010FFFF;

    static inline var UNI_MAX_UTF8_BYTES_PER_CODE_POINT:Int = 4;

    static inline var UNI_UTF16_BYTE_ORDER_MARK_NATIVE:Int=  0xFEFF;
    static inline var UNI_UTF16_BYTE_ORDER_MARK_SWAPPED:Int = 0xFFFE;


	public static inline function isHighSurrogate(codeHigh : Int) : Bool {
		return UNI_SUR_HIGH_START <= codeHigh && codeHigh <= UNI_SUR_HIGH_END;
	}

    public static inline function isSurrogatePair(code : Int) : Bool {
        return code > UNI_MAX_BMP && code <= UNI_MAX_UTF16;
	}

    /* --------------------------------------------------------------------- */

    /*
     * Index into the table below with the first byte of a UTF-8 sequence to
     * get the number of trailing bytes that are supposed to follow it.
     * Note that *legal* UTF-8 values can't have 4 or 5-bytes. The table is
     * left as-is for anyone who may want to do such conversion, which was
     * allowed in earlier algorithms.
     */
    static var trailingBytesForUTF8:Vector<Int> = Vector.fromArrayCopy([
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
    static var offsetsFromUTF8:Vector<Int> = Vector.fromArrayCopy([ // UNSIGNED LONG
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

    public static function charCodeToUtf32ByteAccess(code:Int) {
        var bytes = ByteAccess.alloc(4);
        bytes.setInt32(0, code);
        return bytes;
    }

    public static function charCodeToUtf16ByteAccess (code:Int):ByteAccess {
		var size = getUtf16CodeSize(code);
		var bytes = ByteAccess.alloc(size);
		
		Encoding.writeUtf16CodeBytes(code, 0, 
			function (pos, val) {
				bytes.set(pos << 1, (val >> 8) & 0xFF );
				bytes.set((pos << 1)+1, val & 0xFF); 
			}, StrictConversion, false);
		return bytes;
	}

    public static function convertUcs2toUtf8 (source:Ucs2Reader, flags:ConversionFlags):ByteAccess {

        var target = new ByteAccessBuffer();

        var i = 0;
        
        if (source.length % 2 == 1) throw "invalid source length " + source.length;
        while (i < source.length) { 
            var bytesToWrite:Int = 0;
            
            var ch = source.getInt16(i);
            i+=2;
            /* If we have a surrogate pair, convert to UTF32 first. */
            if (ch >= UNI_SUR_HIGH_START && ch <= UNI_SUR_HIGH_END) {
                throw SourceIllegal(i);
            } else if (flags == StrictConversion) {
                /* UTF-16 surrogate values are illegal in UTF-32 */
                if (ch >= UNI_SUR_LOW_START && ch <= UNI_SUR_LOW_END) {
                    throw SourceIllegal(i-2);
                }
            }

            /* Figure out how many bytes the result will require */
            if (ch < 0x80) {
                bytesToWrite = 1;
            } else if (ch < 0x800) {
                bytesToWrite = 2;
            } else if (ch < 0x10000) {
                bytesToWrite = 3;
            } else if (ch < 0x110000) {
                bytesToWrite = 4;
            } else {
                bytesToWrite = 3;
                ch = UNI_REPLACEMENT_CHAR;
            }

            writeUtf8CodeBytes(ch, bytesToWrite, function (x) target.addByte(x));
            
        }
        
        return target.getByteAccess();
    }

    static inline function writeUtf8CodeBytes (ch:Int, bytesToWrite:Int, addByte:Int->Void) {
        var byteMask:Int = 0xBF;
        var byteMark:Int = 0x80;
        switch (bytesToWrite)
            {
                case 4:
                    addByte( (   (ch >> 18) | firstByteMark[bytesToWrite]) );
                    addByte( ( ( (ch >> 12) | byteMark) & byteMask) );
                    addByte( ( ( (ch >> 6) | byteMark) & byteMask) );
                    addByte( ( ( (ch) | byteMark) & byteMask) );
                case 3:
                    addByte( (   (ch >> 12) | firstByteMark[bytesToWrite]) );
                    addByte( ( ( (ch >> 6) | byteMark) & byteMask) );
                    addByte( ( ( (ch) | byteMark) & byteMask) );
                case 2:
                    addByte( (   (ch >> 6) | firstByteMark[bytesToWrite]) );
                    addByte( ( ( (ch) | byteMark) & byteMask) );
                case 1:
                    addByte( (   (ch) | firstByteMark[bytesToWrite]) );
            }
    }

    public static inline function utf16surrogatePairToCharCode (ch1:Int, ch2:Int) {
        return ((ch1 - UNI_SUR_HIGH_START) << halfShift)
            + (ch2 - UNI_SUR_LOW_START) + halfBase;
    }

    public static function convertUtf16toUtf8 (source:Utf16Reader, flags:ConversionFlags):ByteAccess {

        var target = new ByteAccessBuffer();

        var i = 0;
        
        if (source.length % 2 == 1) throw "invalid source length " + source.length;
        while (i < source.length) { 
            var bytesToWrite:Int = 0;
            var ch = source.getInt16(i);
            i+=2;
            /* If we have a surrogate pair, convert to UTF32 first. */
            if (ch >= UNI_SUR_HIGH_START && ch <= UNI_SUR_HIGH_END) {
                /* If the 16 bits following the high surrogate are in the source buffer... */
                if (i < source.length) {
                    var ch2 = source.getInt16(i);
                    /* If it's a low surrogate, convert to UTF32. */
                    if (ch2 >= UNI_SUR_LOW_START && ch2 <= UNI_SUR_LOW_END) {
                        ch = utf16surrogatePairToCharCode(ch, ch2);
                        i+=2;
                    } else if (flags == StrictConversion) { /* it's an unpaired high surrogate */
                        /* i-=2;  return to the illegal value itself */
                        throw SourceIllegal(i-2);
                    }
                } else { /* We don't have the 16 bits following the high surrogate. */
                    throw SourceExhausted;
                }
            } else if (flags == StrictConversion) {
                /* UTF-16 surrogate values are illegal in UTF-32 */
                if (ch >= UNI_SUR_LOW_START && ch <= UNI_SUR_LOW_END) {
                    throw SourceIllegal(i-2);
                }
            }

            /* Figure out how many bytes the result will require */
            if (ch < 0x80) {
                bytesToWrite = 1;
            } else if (ch < 0x800) {
                bytesToWrite = 2;
            } else if (ch < 0x10000) {
                bytesToWrite = 3;
            } else if (ch < 0x110000) {
                bytesToWrite = 4;
            } else {
                bytesToWrite = 3;
                ch = UNI_REPLACEMENT_CHAR;
            }
            writeUtf8CodeBytes(ch, bytesToWrite, function (x) target.addByte(x));
        }
        
        return target.getByteAccess();
    }

    public static function convertUtf8toUtf16 (source:Utf8Reader, flags:ConversionFlags):ByteAccess {
        return convertUtf8toUtf16OrUcs2(source, flags, false);
    }

    public static function convertUtf8toUcs2 (source:Utf8Reader, flags:ConversionFlags, strictUcs2:Bool):ByteAccess {
        return convertUtf8toUtf16OrUcs2(source, flags, strictUcs2);
    }

    public static function getUtf8CharSize (first) {
        return trailingBytesForUTF8[first]+1;
    }
    

    // asserts valid utf8 bytes
    public static inline function charCodeFromUtf8Bytes (source:ByteAccess, pos:Int, size:Int) {
        var extraBytesToRead:Int = size - 1;

        var ch = 0;
        switch (size) {
            case 4:
                ch += source.fastGet(pos++); ch <<= 6;
                ch += source.fastGet(pos++); ch <<= 6;
                ch += source.fastGet(pos++); ch <<= 6;
                ch += source.fastGet(pos++);
            case 3:
                ch += source.fastGet(pos++); ch <<= 6;
                ch += source.fastGet(pos++); ch <<= 6;
                ch += source.fastGet(pos++);
            case 2:
                ch += source.fastGet(pos++); ch <<= 6;
                ch += source.fastGet(pos++);
            case 1:
                ch += source.fastGet(pos++);
            default:
        }
        ch -= offsetsFromUTF8[extraBytesToRead];
        return ch;

    } 

    public static function convertUtf8toUtf16OrUcs2 (source:Utf8Reader, flags:ConversionFlags, strictUcs2:Bool):ByteAccess {

        var target = new ByteAccessBuffer();

        var i = 0;

        while (i < source.length) {

            var ch = 0;
            var extraBytesToRead:Int = trailingBytesForUTF8[source.fastGet(i)];



            if (extraBytesToRead >= source.length - i) {
                throw SourceExhausted;
            }
            /* Do this check whether lenient or strict */
            if (!isLegalUTF8(source, i, extraBytesToRead+1)) {
                throw SourceIllegal(i);
            }
            /*
             * The cases all fall through. See "Note A" below.
             */

            var j = i;
            if (extraBytesToRead >= 5) { ch += source.fastGet(j++); ch <<= 6;} /* remember, illegal UTF-8 */
            if (extraBytesToRead >= 4) { ch += source.fastGet(j++); ch <<= 6;} /* remember, illegal UTF-8 */
            if (extraBytesToRead >= 3) { ch += source.fastGet(j++); ch <<= 6;}
            if (extraBytesToRead >= 2) { ch += source.fastGet(j++); ch <<= 6;}
            if (extraBytesToRead >= 1) { ch += source.fastGet(j++); ch <<= 6;}
            if (extraBytesToRead >= 0) { ch += source.fastGet(j++); }

            ch -= offsetsFromUTF8[extraBytesToRead];

            
            writeUtf16CodeBytes(ch, i, function (_, ch) target.addInt16BigEndian(ch), flags, strictUcs2 );
            i+=extraBytesToRead+1;
        }
        return target.getByteAccess();
    }

    public static inline function getUtf16CodeSize (code:Int):Int {
		return if (code <= UNI_MAX_BMP) 2 else 4;
	}

    public inline static function writeUtf16CodeBytes(ch:Int, pos:Int, addInt16:Int->Int->Void, flags:ConversionFlags, strictUcs2:Bool) {
        if (ch <= UNI_MAX_BMP) { /* Target is a character <= 0xFFFF */
            /* UTF-16 surrogate values are illegal in UTF-32 */
            if (ch >= UNI_SUR_HIGH_START && ch <= UNI_SUR_LOW_END) {
                if (flags == StrictConversion) {
                    throw SourceIllegal(pos);
                } else {
                    addInt16(pos, UNI_REPLACEMENT_CHAR);
                }
            } else {
                addInt16(pos, ch);
            }
        } else if (ch > UNI_MAX_UTF16) {
            if (flags == StrictConversion) {
                throw SourceIllegal(pos);
            } else {
                addInt16(pos, UNI_REPLACEMENT_CHAR);
            }
        } else {
            if (strictUcs2) {
                throw SourceIllegal(pos);
            } else {
                var s = codeToSurrogatePair(ch);
                addInt16(pos, s.high);
                addInt16(pos+1, s.low);
            }
        }

    }

    public inline static function codeToSurrogatePair (code:Int) {
        var code = code - halfBase;
        var high = ((code >> halfShift) + UNI_SUR_HIGH_START);
        var low = ((code & halfMask) + UNI_SUR_LOW_START);
        return { high : high, low : low };
    }

    public static function isLegalUtf8String(source:Utf8Reader):Bool {
        var i = 0;

        while (i < source.length) {
            var length = trailingBytesForUTF8[source.fastGet(i)] + 1;
            if (i + length > source.length || !isLegalUTF8(source, i, length))
                return false;
            i += length;
        }
        return true;
    }
    /*
     * Utility routine to tell whether a sequence of bytes is legal UTF-8.
     * This must be called with the length pre-determined by the first byte.
     * If not calling this from ConvertUTF8to*, then the length can be set by:
     *  length = trailingBytesForUTF8[*source]+1;
     * and the sequence is illegal right away if there aren't that many bytes
     * available.
     * If presented with a length > 4, this returns false.  The Unicode
     * definition of UTF-8 goes up to 4-byte sequences.
     */

    static function isLegalUTF8( source:Utf8Reader, pos:Int, length:Int):Bool {

        var ch0 = source.fastGet(pos);

        inline function case4 () {
            var ch3 = source.fastGet(pos+3);
            return if (ch3 < 0x80 || ch3 > 0xBF) false else true;
        }

        inline function case3 () {
            var ch2 = source.fastGet(pos+2);
            return if (ch2 < 0x80 || ch2 > 0xBF) false else true;
        }

        inline function case2 () {
            var ch1 = source.fastGet(pos+1);
            return if (ch1 < 0x80 || ch1 > 0xBF) {
                false;
            } else switch (ch0) {
                case 0xE0 if (ch1 < 0xA0): false;
                case 0xED if (ch1 > 0x9F): false;
                case 0xF0 if (ch1 < 0x90): false;
                case 0xF4 if (ch1 > 0x8F): false;
                case _    if (ch1 < 0x80): false;
                case _: true;
            }
        }

        inline function case1 () {
            return if (ch0 >= 0x80 && ch0 < 0xC2) false else true;
        }

        inline function case0 () {
            return if (ch0 > 0xF4) false else true;
        }

        return switch (length)
        {
            case 4:
                case4() && case3() && case2() && case1() && case0();
            case 3:
                case3() && case2() && case1() && case0();
            case 2:
                case2() && case1() && case0();
            case 1:
                case1() && case0();
            case _: false;
        }
    }

    static function findMaximalSubpartOfIllFormedUTF8Sequence(source:Utf8Reader, pos:Int) 
    {
        var sourceEnd = source.length;

        
        // assert(!isLegalUTF8Sequence(source, sourceEnd));

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
        // assert((b1 >= 0x80 && b1 <= 0xC1) || b1 >= 0xF5);
        /*
        * There are no valid sequences that start with these bytes.  Maximal subpart
        * is defined to have length 1 in these cases.
        */
        return 1;
    }

    static function convertUtf8toUtf32Impl(source:Utf8Reader, flags:ConversionFlags, inputIsPartial:Bool):ByteAccess 
    {
        var i = 0;
        
        var target = new ByteAccessBuffer(); 

        while (i < source.length) {
            var ch:Int = 0;
            var extraBytesToRead = trailingBytesForUTF8[source.fastGet(i)];
            if (extraBytesToRead >= source.length - i) {
                if (flags == StrictConversion || inputIsPartial) {
                    throw SourceExhausted;
                } else {

                    //result = sourceIllegal;

                    /*
                    * Replace the maximal subpart of ill-formed sequence with
                    * replacement character.
                    */
                    i += findMaximalSubpartOfIllFormedUTF8Sequence(source, i);
                    target.addInt32BigEndian(UNI_REPLACEMENT_CHAR);
                    continue;
                }
            }

            /* Do this check whether lenient or strict */
            if (!isLegalUTF8(source, i, extraBytesToRead+1)) {
                if (flags == StrictConversion) {
                    /* Abort conversion. */
                    throw SourceIllegal(i);
                } else {
                    /*
                    * Replace the maximal subpart of ill-formed sequence with
                    * replacement character.
                    */
                    i += findMaximalSubpartOfIllFormedUTF8Sequence(source, i);
                    target.addInt32BigEndian(UNI_REPLACEMENT_CHAR);
                    continue;
                }
            }
            /*
            * The cases all fall through. See "Note A" below.
            */
            if (extraBytesToRead >= 5) { ch += source.fastGet(i++); ch <<= 6;} /* remember, illegal UTF-8 */
            if (extraBytesToRead >= 4) { ch += source.fastGet(i++); ch <<= 6;} /* remember, illegal UTF-8 */
            if (extraBytesToRead >= 3) { ch += source.fastGet(i++); ch <<= 6;}
            if (extraBytesToRead >= 2) { ch += source.fastGet(i++); ch <<= 6;}
            if (extraBytesToRead >= 1) { ch += source.fastGet(i++); ch <<= 6;}
            if (extraBytesToRead >= 0) { ch += source.fastGet(i++); }
            
            ch -= offsetsFromUTF8[extraBytesToRead];

            if (ch <= UNI_MAX_LEGAL_UTF32) {
                /*
                * UTF-16 surrogate values are illegal in UTF-32, and anything
                * over Plane 17 (> 0x10FFFF) is illegal.
                */
                if (ch >= UNI_SUR_HIGH_START && ch <= UNI_SUR_LOW_END) {
                    if (flags == StrictConversion) {
                        throw SourceIllegal(i);
                    } else {
                        target.addInt32BigEndian(UNI_REPLACEMENT_CHAR);
                    }
                } else {
                    target.addInt32BigEndian(ch);
                }
            } else { /* i.e., ch > UNI_MAX_LEGAL_UTF32 */
                throw SourceIllegal(i);
            }
        }
        
        return target.getByteAccess();
    }

    static function convertUtf8toUtf32Partial(source:Utf8Reader, flags:ConversionFlags):ByteAccess {
        return convertUtf8toUtf32Impl(source, flags, /*InputIsPartial=*/true);
    }

    public static function convertUtf8toUtf32(source:Utf8Reader, flags:ConversionFlags):ByteAccess {
        return convertUtf8toUtf32Impl(source,flags, /*InputIsPartial=*/false);
    }

    public static function convertUtf32toUtf8 (source:Utf32Reader, flags:ConversionFlags):ByteAccess 
    {
        var i = 0;
        
        var target = new ByteAccessBuffer(); 
        while (i < source.length) {
            var ch:Int = 0;
            
            var bytesToWrite = 0;
            var byteMask = 0xBF;
            var byteMark = 0x80;

            ch = source.getInt32(i);


            if (flags == StrictConversion ) {
                /* UTF-16 surrogate values are illegal in UTF-32 */
                if (ch >= UNI_SUR_HIGH_START && ch <= UNI_SUR_LOW_END) {
                    trace("here we go");
                    throw SourceIllegal(i);
                }
            }
            /*
            * Figure out how many bytes the result will require. Turn any
            * illegally large UTF32 things (> Plane 17) into replacement chars.
            */

            if (ch < 0x80) {
                bytesToWrite = 1;
            } else if (ch < 0x800) {
                bytesToWrite = 2;
            } else if (ch < 0x10000) {
                bytesToWrite = 3;
            } else if (ch <= UNI_MAX_LEGAL_UTF32) {
                bytesToWrite = 4;
            } else {
                bytesToWrite = 3;
                ch = UNI_REPLACEMENT_CHAR;
                if (flags == StrictConversion) {

                    trace("source illegal");
                    throw SourceIllegal(i);
                }
            }

            switch (bytesToWrite) {
                case 4: 
                    target.addByte( (ch >> 18) | firstByteMark[bytesToWrite]);
                    target.addByte(( (ch >> 12) | byteMark) & byteMask); 
                    target.addByte(( (ch >> 6) | byteMark) & byteMask); 
                    target.addByte((ch | byteMark) & byteMask); 
                case 3: 
                    target.addByte( (ch >> 12) | firstByteMark[bytesToWrite]);
                    target.addByte(( (ch >> 6) | byteMark) & byteMask); 
                    target.addByte((ch | byteMark) & byteMask); 
                case 2: 
                    target.addByte( (ch >> 6) | firstByteMark[bytesToWrite]);
                    target.addByte((ch | byteMark) & byteMask); 
                case 1: 
                    target.addByte(ch | firstByteMark[bytesToWrite]);
                case _:
            }
            i+=4;
            
        }
        return target.getByteAccess();
    }

    public static function convertUtf32toUtf16 (source:Utf32Reader, flags:ConversionFlags) 
    {
        var i = 0;
        var target = new ByteAccessBuffer();

        while (i < source.length) {
            var ch = 0;
            
            ch = source.getInt32(i);
            if (ch <= UNI_MAX_BMP) { /* Target is a character <= 0xFFFF */
                /* UTF-16 surrogate values are illegal in UTF-32; 0xffff or 0xfffe are both reserved values */
                if (ch >= UNI_SUR_HIGH_START && ch <= UNI_SUR_LOW_END) {
                    if (flags == StrictConversion) {
                        throw SourceIllegal(i);
                    } else {
                        target.addInt16BigEndian(UNI_REPLACEMENT_CHAR);
                    }
                } else {
                    target.addInt16BigEndian(ch);
                }
            } else if (ch > UNI_MAX_LEGAL_UTF32) {
                if (flags == StrictConversion) {
                    throw SourceIllegal(i);
                } else {
                    target.addInt16BigEndian(UNI_REPLACEMENT_CHAR);
                }
            } else {
                ch -= halfBase;
                target.addInt16BigEndian((ch >> halfShift) + UNI_SUR_HIGH_START);
                target.addInt16BigEndian((ch & halfMask) + UNI_SUR_LOW_START);
            }
            i+=4;
        }
        return target.getByteAccess();
    }

    public static function convertUtf16toUtf32 (source:Utf16Reader, flags:ConversionFlags) {
        
        var i = 0;
        var target = new ByteAccessBuffer();

        var ch = 0;
        var ch2 = 0;
        
        while (i < source.length) 
        {

            //const UTF16* oldSource = source; /*  In case we have to back up because of target overflow. */
            ch = source.getInt16(i);
            

            /* If we have a surrogate pair, convert to UTF32 first. */
            if (ch >= UNI_SUR_HIGH_START && ch <= UNI_SUR_HIGH_END) {
                /* If the 16 bits following the high surrogate are in the source buffer... */
                if (i < source.length-2) {
                    ch2 = source.getInt16(i+2);
                    /* If it's a low surrogate, convert to UTF32. */
                    if (ch2 >= UNI_SUR_LOW_START && ch2 <= UNI_SUR_LOW_END) {
                        ch = ((ch - UNI_SUR_HIGH_START) << halfShift)
                            + (ch2 - UNI_SUR_LOW_START) + halfBase;
                        i+=2;
                    } else if (flags == StrictConversion) { /* it's an unpaired high surrogate */
                        throw SourceIllegal(i);
                    }
                } else { /* We don't have the 16 bits following the high surrogate. */
                    throw SourceExhausted;
                }
            } else if (flags == StrictConversion) {
                /* UTF-16 surrogate values are illegal in UTF-32 */
                if (ch >= UNI_SUR_LOW_START && ch <= UNI_SUR_LOW_END) {
                    throw SourceIllegal(i);
                }
            }
            target.addInt32BigEndian(ch);
            i+=2;
        }
    
        return target.getByteAccess();
    }
}