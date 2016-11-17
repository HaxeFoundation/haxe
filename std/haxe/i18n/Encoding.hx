package haxe.i18n;
import haxe.i18n.ByteAccess;
import haxe.i18n.ByteAccessBuffer;



/***** SOURCE
// http://llvm.org/svn/llvm-project/llvm/trunk/include/llvm/Support/ConvertUTF.h
// http://llvm.org/svn/llvm-project/llvm/trunk/lib/Support/ConvertUTF.c

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


enum ConversionError {
    SourceExhausted;
    SourceIllegal(bytePos:Int);
}

enum ConversionFlags {
  StrictConversion;
  LenientConversion;
}


class Encoding {
    static var halfShift:Int  = 10; /* used for shifting by 10 bits */

    static var halfBase:Int = 0x0010000;
    static var halfMask:Int = 0x3FF;

    static var UNI_SUR_HIGH_START:Int = 0xD800;
    static var UNI_SUR_HIGH_END:Int = 0xDBFF;

    static var UNI_SUR_LOW_START:Int = 0xDC00;
    static var UNI_SUR_LOW_END:Int = 0xDFFF;

    static var UNI_REPLACEMENT_CHAR:Int = 0x0000FFFD;
    static var UNI_MAX_BMP:Int = 0x0000FFFF;
    static var UNI_MAX_UTF16:Int = 0x0010FFFF;
    static var UNI_MAX_UTF32:Int = 0x7FFFFFFF;
    static var UNI_MAX_LEGAL_UTF32:Int = 0x0010FFFF;

    static var UNI_MAX_UTF8_BYTES_PER_CODE_POINT:Int = 4;

    static var UNI_UTF16_BYTE_ORDER_MARK_NATIVE:Int=  0xFEFF;
    static var UNI_UTF16_BYTE_ORDER_MARK_SWAPPED:Int = 0xFFFE;

    /* --------------------------------------------------------------------- */

    /*
     * Index into the table below with the first byte of a UTF-8 sequence to
     * get the number of trailing bytes that are supposed to follow it.
     * Note that *legal* UTF-8 values can't have 4 or 5-bytes. The table is
     * left as-is for anyone who may want to do such conversion, which was
     * allowed in earlier algorithms.
     */
    static var trailingBytesForUTF8:Array<Int> = [
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
        2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, 3,3,3,3,3,3,3,3,4,4,4,4,5,5,5,5
    ];


    /*
     * Magic values subtracted from a buffer value during UTF8 conversion.
     * This table contains as many values as there might be trailing bytes
     * in a UTF-8 sequence.
     */
    static var offsetsFromUTF8:Array<Int> = [ // UNSIGNED LONG
        0x00000000, 0x00003080, 0x000E2080,
        0x03C82080, 0xFA082080, 0x82082080
    ];

    /*
     * Once the bits are split out into bytes of UTF-8, this is a mask OR-ed
     * into the first byte, depending on how many bytes follow.  There are
     * as many entries in this table as there are UTF-8 sequence types.
     * (I.e., one byte sequence, two byte... etc.). Remember that sequencs
     * for *legal* UTF-8 will be 4 or fewer bytes total.
     */
    static var firstByteMark:Array<Int> = [
        0x00, 0x00, 0xC0, 0xE0, 0xF0, 0xF8, 0xFC
    ];


    public static function convertUTF16toUTF8 (source:Utf16Reader, flags:ConversionFlags):ByteAccess {

        var target = new ByteAccessBuffer();

        var i = 0;
        
        if (source.length % 2 == 1) throw "invalid source length " + source.length;
        while (i < source.length) { 
            var bytesToWrite:Int = 0;
            var byteMask:Int = 0xBF;
            var byteMark:Int = 0x80;
            var ch = source.getInt16(i);
            i+=2;
            /* If we have a surrogate pair, convert to UTF32 first. */
            if (ch >= UNI_SUR_HIGH_START && ch <= UNI_SUR_HIGH_END) {
                /* If the 16 bits following the high surrogate are in the source buffer... */
                if (i < source.length) {
                    var ch2 = source.getInt16(i);
                    /* If it's a low surrogate, convert to UTF32. */
                    if (ch2 >= UNI_SUR_LOW_START && ch2 <= UNI_SUR_LOW_END) {
                        ch = ((ch - UNI_SUR_HIGH_START) << halfShift)
                            + (ch2 - UNI_SUR_LOW_START) + halfBase;
                        i+=2;
                    } else if (flags.match(StrictConversion)) { /* it's an unpaired high surrogate */
                        /* i-=2;  return to the illegal value itself */
                        throw SourceIllegal(i-2);
                    }
                } else { /* We don't have the 16 bits following the high surrogate. */
                    throw SourceExhausted;
                }
            } else if (flags.match(StrictConversion)) {
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

            switch (bytesToWrite)
            {
                case 4:
                    target.addByte( (   (ch >> 18) | firstByteMark[bytesToWrite]) );
                    target.addByte( ( ( (ch >> 12) | byteMark) & byteMask) );
                    target.addByte( ( ( (ch >> 6) | byteMark) & byteMask) );
                    target.addByte( ( ( (ch) | byteMark) & byteMask) );
                case 3:
                    target.addByte( (   (ch >> 12) | firstByteMark[bytesToWrite]) );
                    target.addByte( ( ( (ch >> 6) | byteMark) & byteMask) );
                    target.addByte( ( ( (ch) | byteMark) & byteMask) );
                case 2:
                    target.addByte( (   (ch >> 6) | firstByteMark[bytesToWrite]) );
                    target.addByte( ( ( (ch) | byteMark) & byteMask) );
                case 1:
                    target.addByte( (   (ch) | firstByteMark[bytesToWrite]) );
            }
        }
        
        return target.getByteAccess();
    }

    

    public static function convertUtf8toUtf16 (source:Utf8Reader, flags:ConversionFlags):ByteAccess {
        return convertUtf8toUtf16OrUcs2(source, flags, false);
    }
    public static function convertUtf8toUcs2 (source:Utf8Reader, flags:ConversionFlags):ByteAccess {
        return convertUtf8toUtf16OrUcs2(source, flags, true);
    }

    public static function getUtf8CharSize (first) {
        return trailingBytesForUTF8[first]+1;
    }

    public static function convertUtf8toUtf16OrUcs2 (source:Utf8Reader, flags:ConversionFlags, toUcs2:Bool):ByteAccess {

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

            if (ch <= UNI_MAX_BMP) { /* Target is a character <= 0xFFFF */
                /* UTF-16 surrogate values are illegal in UTF-32 */
                if (ch >= UNI_SUR_HIGH_START && ch <= UNI_SUR_LOW_END) {
                    if (flags.match(StrictConversion)) {
                        throw SourceIllegal(i);
                    } else {
                        target.addByte(UNI_REPLACEMENT_CHAR);
                    }
                } else {
                    target.addInt16BigEndian(ch);
                }
            } else if (ch > UNI_MAX_UTF16) {
                if (flags.match(StrictConversion)) {
                    throw SourceIllegal(i);
                } else {
                    target.addInt16BigEndian(UNI_REPLACEMENT_CHAR);
                }
            } else {
                if (toUcs2) {
                    throw SourceIllegal(i);
                } else {
                    /* target is a character in range 0xFFFF - 0x10FFFF. */
                    ch -= halfBase;
                    target.addInt16BigEndian(((ch >> halfShift) + UNI_SUR_HIGH_START));
                    target.addInt16BigEndian(((ch & halfMask) + UNI_SUR_LOW_START));
                }
            }
            i+=extraBytesToRead+1;
        }
        return target.getByteAccess();
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
}



/* --------------------------------------------------------------------- */

/* The interface converts a whole buffer to avoid function-call overhead.
 * Constants have been gathered. Loops & conditionals have been removed as
 * much as possible for efficiency, in favor of drop-through switches.
 * (See "Note A" at the bottom of the file for equivalent code.)
 * If your compiler supports it, the "isLegalUTF8" call can be turned
 * into an inline function.
 */


/* --------------------------------------------------------------------- */
#if false
ConversionResult ConvertUTF32toUTF16 (
        const UTF32** sourceStart, const UTF32* sourceEnd,
        UTF16** targetStart, UTF16* targetEnd, ConversionFlags flags) {
    ConversionResult result = conversionOK;
    const UTF32* source = *sourceStart;
    UTF16* target = *targetStart;
    while (source < sourceEnd) {
        UTF32 ch;
        if (target >= targetEnd) {
            result = targetExhausted; break;
        }
        ch = *source++;
        if (ch <= UNI_MAX_BMP) { /* Target is a character <= 0xFFFF */
            /* UTF-16 surrogate values are illegal in UTF-32; 0xffff or 0xfffe are both reserved values */
            if (ch >= UNI_SUR_HIGH_START && ch <= UNI_SUR_LOW_END) {
                if (flags == strictConversion) {
                    --source; /* return to the illegal value itself */
                    result = sourceIllegal;
                    break;
                } else {
                    *target++ = UNI_REPLACEMENT_CHAR;
                }
            } else {
                *target++ = (UTF16)ch; /* normal case */
            }
        } else if (ch > UNI_MAX_LEGAL_UTF32) {
            if (flags == strictConversion) {
                result = sourceIllegal;
            } else {
                *target++ = UNI_REPLACEMENT_CHAR;
            }
        } else {
            /* target is a character in range 0xFFFF - 0x10FFFF. */
            if (target + 1 >= targetEnd) {
                --source; /* Back up source pointer! */
                result = targetExhausted; break;
            }
            ch -= halfBase;
            *target++ = (UTF16)((ch >> halfShift) + UNI_SUR_HIGH_START);
            *target++ = (UTF16)((ch & halfMask) + UNI_SUR_LOW_START);
        }
    }
    *sourceStart = source;
    *targetStart = target;
    return result;
}

/* --------------------------------------------------------------------- */

ConversionResult ConvertUTF16toUTF32 (
        const UTF16** sourceStart, const UTF16* sourceEnd,
        UTF32** targetStart, UTF32* targetEnd, ConversionFlags flags) {
    ConversionResult result = conversionOK;
    const UTF16* source = *sourceStart;
    UTF32* target = *targetStart;
    UTF32 ch, ch2;
    while (source < sourceEnd) {
        const UTF16* oldSource = source; /*  In case we have to back up because of target overflow. */
        ch = *source++;
        /* If we have a surrogate pair, convert to UTF32 first. */
        if (ch >= UNI_SUR_HIGH_START && ch <= UNI_SUR_HIGH_END) {
            /* If the 16 bits following the high surrogate are in the source buffer... */
            if (source < sourceEnd) {
                ch2 = *source;
                /* If it's a low surrogate, convert to UTF32. */
                if (ch2 >= UNI_SUR_LOW_START && ch2 <= UNI_SUR_LOW_END) {
                    ch = ((ch - UNI_SUR_HIGH_START) << halfShift)
                        + (ch2 - UNI_SUR_LOW_START) + halfBase;
                    ++source;
                } else if (flags == strictConversion) { /* it's an unpaired high surrogate */
                    --source; /* return to the illegal value itself */
                    result = sourceIllegal;
                    break;
                }
            } else { /* We don't have the 16 bits following the high surrogate. */
                --source; /* return to the high surrogate */
                result = sourceExhausted;
                break;
            }
        } else if (flags == strictConversion) {
            /* UTF-16 surrogate values are illegal in UTF-32 */
            if (ch >= UNI_SUR_LOW_START && ch <= UNI_SUR_LOW_END) {
                --source; /* return to the illegal value itself */
                result = sourceIllegal;
                break;
            }
        }
        if (target >= targetEnd) {
            source = oldSource; /* Back up source pointer! */
            result = targetExhausted; break;
        }
        *target++ = ch;
    }
    *sourceStart = source;
    *targetStart = target;
#ifdef CVTUTF_DEBUG
if (result == sourceIllegal) {
    fprintf(stderr, "ConvertUTF16toUTF32 illegal seq 0x%04x,%04x\n", ch, ch2);
    fflush(stderr);
}
#endif
    return result;
}


/* --------------------------------------------------------------------- */

ConversionResult ConvertUTF32toUTF8 (
        const UTF32** sourceStart, const UTF32* sourceEnd,
        UTF8** targetStart, UTF8* targetEnd, ConversionFlags flags) {
    ConversionResult result = conversionOK;
    const UTF32* source = *sourceStart;
    UTF8* target = *targetStart;
    while (source < sourceEnd) {
        UTF32 ch;
        unsigned short bytesToWrite = 0;
        const UTF32 byteMask = 0xBF;
        const UTF32 byteMark = 0x80;
        ch = *source++;
        if (flags == strictConversion ) {
            /* UTF-16 surrogate values are illegal in UTF-32 */
            if (ch >= UNI_SUR_HIGH_START && ch <= UNI_SUR_LOW_END) {
                --source; /* return to the illegal value itself */
                result = sourceIllegal;
                break;
            }
        }
        /*
         * Figure out how many bytes the result will require. Turn any
         * illegally large UTF32 things (> Plane 17) into replacement chars.
         */
        if (ch < (UTF32)0x80) {      bytesToWrite = 1;
        } else if (ch < (UTF32)0x800) {     bytesToWrite = 2;
        } else if (ch < (UTF32)0x10000) {   bytesToWrite = 3;
        } else if (ch <= UNI_MAX_LEGAL_UTF32) {  bytesToWrite = 4;
        } else {                            bytesToWrite = 3;
                                            ch = UNI_REPLACEMENT_CHAR;
                                            result = sourceIllegal;
        }

        target += bytesToWrite;
        if (target > targetEnd) {
            --source; /* Back up source pointer! */
            target -= bytesToWrite; result = targetExhausted; break;
        }
        switch (bytesToWrite) { /* note: everything falls through. */
            case 4: *--target = (UTF8)((ch | byteMark) & byteMask); ch >>= 6;
            case 3: *--target = (UTF8)((ch | byteMark) & byteMask); ch >>= 6;
            case 2: *--target = (UTF8)((ch | byteMark) & byteMask); ch >>= 6;
            case 1: *--target = (UTF8) (ch | firstByteMark[bytesToWrite]);
        }
        target += bytesToWrite;
    }
    *sourceStart = source;
    *targetStart = target;
    return result;
}

/* --------------------------------------------------------------------- */


/* --------------------------------------------------------------------- */

/*
 * Exported function to return whether a UTF-8 sequence is legal or not.
 * This is not used here; it's just exported.
 */
Boolean isLegalUTF8Sequence(const UTF8 *source, const UTF8 *sourceEnd) {
    int length = trailingBytesForUTF8[*source]+1;
    if (length > sourceEnd - source) {
        return false;
    }
    return isLegalUTF8(source, length);
}

/* --------------------------------------------------------------------- */

static unsigned
findMaximalSubpartOfIllFormedUTF8Sequence(const UTF8 *source,
                                          const UTF8 *sourceEnd) {
  UTF8 b1, b2, b3;

  assert(!isLegalUTF8Sequence(source, sourceEnd));

  /*
   * Unicode 6.3.0, D93b:
   *
   *   Maximal subpart of an ill-formed subsequence: The longest code unit
   *   subsequence starting at an unconvertible offset that is either:
   *   a. the initial subsequence of a well-formed code unit sequence, or
   *   b. a subsequence of length one.
   */

  if (source == sourceEnd)
    return 0;

  /*
   * Perform case analysis.  See Unicode 6.3.0, Table 3-7. Well-Formed UTF-8
   * Byte Sequences.
   */

  b1 = *source;
  ++source;
  if (b1 >= 0xC2 && b1 <= 0xDF) {
    /*
     * First byte is valid, but we know that this code unit sequence is
     * invalid, so the maximal subpart has to end after the first byte.
     */
    return 1;
  }

  if (source == sourceEnd)
    return 1;

  b2 = *source;
  ++source;

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
      if (source == sourceEnd)
        return 2;

      b3 = *source;
      return (b3 >= 0x80 && b3 <= 0xBF) ? 3 : 2;
    }
    return 1;
  }
  if (b1 >= 0xF1 && b1 <= 0xF3) {
    if (b2 >= 0x80 && b2 <= 0xBF) {
      if (source == sourceEnd)
        return 2;

      b3 = *source;
      return (b3 >= 0x80 && b3 <= 0xBF) ? 3 : 2;
    }
    return 1;
  }
  if (b1 == 0xF4) {
    if (b2 >= 0x80 && b2 <= 0x8F) {
      if (source == sourceEnd)
        return 2;

      b3 = *source;
      return (b3 >= 0x80 && b3 <= 0xBF) ? 3 : 2;
    }
    return 1;
  }

  assert((b1 >= 0x80 && b1 <= 0xC1) || b1 >= 0xF5);
  /*
   * There are no valid sequences that start with these bytes.  Maximal subpart
   * is defined to have length 1 in these cases.
   */
  return 1;
}

/* --------------------------------------------------------------------- */

/*
 * Exported function to return the total number of bytes in a codepoint
 * represented in UTF-8, given the value of the first byte.
 */
unsigned getNumBytesForUTF8(UTF8 first) {
  return trailingBytesForUTF8[first] + 1;
}

/* --------------------------------------------------------------------- */

/*
 * Exported function to return whether a UTF-8 string is legal or not.
 * This is not used here; it's just exported.
 */
Boolean isLegalUTF8String(const UTF8 **source, const UTF8 *sourceEnd) {
    while (*source != sourceEnd) {
        int length = trailingBytesForUTF8[**source] + 1;
        if (length > sourceEnd - *source || !isLegalUTF8(*source, length))
            return false;
        *source += length;
    }
    return true;
}

/* --------------------------------------------------------------------- */

ConversionResult ConvertUTF8toUTF16 (
        const UTF8** sourceStart, const UTF8* sourceEnd,
        UTF16** targetStart, UTF16* targetEnd, ConversionFlags flags) {
    ConversionResult result = conversionOK;
    const UTF8* source = *sourceStart;
    UTF16* target = *targetStart;
    while (source < sourceEnd) {
        UTF32 ch = 0;
        unsigned short extraBytesToRead = trailingBytesForUTF8[*source];
        if (extraBytesToRead >= sourceEnd - source) {
            result = sourceExhausted; break;
        }
        /* Do this check whether lenient or strict */
        if (!isLegalUTF8(source, extraBytesToRead+1)) {
            result = sourceIllegal;
            break;
        }
        /*
         * The cases all fall through. See "Note A" below.
         */
        switch (extraBytesToRead) {
            case 5: ch += *source++; ch <<= 6; /* remember, illegal UTF-8 */
            case 4: ch += *source++; ch <<= 6; /* remember, illegal UTF-8 */
            case 3: ch += *source++; ch <<= 6;
            case 2: ch += *source++; ch <<= 6;
            case 1: ch += *source++; ch <<= 6;
            case 0: ch += *source++;
        }
        ch -= offsetsFromUTF8[extraBytesToRead];

        if (target >= targetEnd) {
            source -= (extraBytesToRead+1); /* Back up source pointer! */
            result = targetExhausted; break;
        }
        if (ch <= UNI_MAX_BMP) { /* Target is a character <= 0xFFFF */
            /* UTF-16 surrogate values are illegal in UTF-32 */
            if (ch >= UNI_SUR_HIGH_START && ch <= UNI_SUR_LOW_END) {
                if (flags == strictConversion) {
                    source -= (extraBytesToRead+1); /* return to the illegal value itself */
                    result = sourceIllegal;
                    break;
                } else {
                    *target++ = UNI_REPLACEMENT_CHAR;
                }
            } else {
                *target++ = (UTF16)ch; /* normal case */
            }
        } else if (ch > UNI_MAX_UTF16) {
            if (flags == strictConversion) {
                result = sourceIllegal;
                source -= (extraBytesToRead+1); /* return to the start */
                break; /* Bail out; shouldn't continue */
            } else {
                *target++ = UNI_REPLACEMENT_CHAR;
            }
        } else {
            /* target is a character in range 0xFFFF - 0x10FFFF. */
            if (target + 1 >= targetEnd) {
                source -= (extraBytesToRead+1); /* Back up source pointer! */
                result = targetExhausted; break;
            }
            ch -= halfBase;
            *target++ = (UTF16)((ch >> halfShift) + UNI_SUR_HIGH_START);
            *target++ = (UTF16)((ch & halfMask) + UNI_SUR_LOW_START);
        }
    }
    *sourceStart = source;
    *targetStart = target;
    return result;
}

/* --------------------------------------------------------------------- */

static ConversionResult ConvertUTF8toUTF32Impl(
        const UTF8** sourceStart, const UTF8* sourceEnd,
        UTF32** targetStart, UTF32* targetEnd, ConversionFlags flags,
        Boolean InputIsPartial) {
    ConversionResult result = conversionOK;
    const UTF8* source = *sourceStart;
    UTF32* target = *targetStart;
    while (source < sourceEnd) {
        UTF32 ch = 0;
        unsigned short extraBytesToRead = trailingBytesForUTF8[*source];
        if (extraBytesToRead >= sourceEnd - source) {
            if (flags == strictConversion || InputIsPartial) {
                result = sourceExhausted;
                break;
            } else {
                result = sourceIllegal;

                /*
                 * Replace the maximal subpart of ill-formed sequence with
                 * replacement character.
                 */
                source += findMaximalSubpartOfIllFormedUTF8Sequence(source,
                                                                    sourceEnd);
                *target++ = UNI_REPLACEMENT_CHAR;
                continue;
            }
        }
        if (target >= targetEnd) {
            result = targetExhausted; break;
        }

        /* Do this check whether lenient or strict */
        if (!isLegalUTF8(source, extraBytesToRead+1)) {
            result = sourceIllegal;
            if (flags == strictConversion) {
                /* Abort conversion. */
                break;
            } else {
                /*
                 * Replace the maximal subpart of ill-formed sequence with
                 * replacement character.
                 */
                source += findMaximalSubpartOfIllFormedUTF8Sequence(source,
                                                                    sourceEnd);
                *target++ = UNI_REPLACEMENT_CHAR;
                continue;
            }
        }
        /*
         * The cases all fall through. See "Note A" below.
         */
        switch (extraBytesToRead) {
            case 5: ch += *source++; ch <<= 6;
            case 4: ch += *source++; ch <<= 6;
            case 3: ch += *source++; ch <<= 6;
            case 2: ch += *source++; ch <<= 6;
            case 1: ch += *source++; ch <<= 6;
            case 0: ch += *source++;
        }
        ch -= offsetsFromUTF8[extraBytesToRead];

        if (ch <= UNI_MAX_LEGAL_UTF32) {
            /*
             * UTF-16 surrogate values are illegal in UTF-32, and anything
             * over Plane 17 (> 0x10FFFF) is illegal.
             */
            if (ch >= UNI_SUR_HIGH_START && ch <= UNI_SUR_LOW_END) {
                if (flags == strictConversion) {
                    source -= (extraBytesToRead+1); /* return to the illegal value itself */
                    result = sourceIllegal;
                    break;
                } else {
                    *target++ = UNI_REPLACEMENT_CHAR;
                }
            } else {
                *target++ = ch;
            }
        } else { /* i.e., ch > UNI_MAX_LEGAL_UTF32 */
            result = sourceIllegal;
            *target++ = UNI_REPLACEMENT_CHAR;
        }
    }
    *sourceStart = source;
    *targetStart = target;
    return result;
}

ConversionResult ConvertUTF8toUTF32Partial(const UTF8 **sourceStart,
                                           const UTF8 *sourceEnd,
                                           UTF32 **targetStart,
                                           UTF32 *targetEnd,
                                           ConversionFlags flags) {
  return ConvertUTF8toUTF32Impl(sourceStart, sourceEnd, targetStart, targetEnd,
                                flags, /*InputIsPartial=*/true);
}

ConversionResult ConvertUTF8toUTF32(const UTF8 **sourceStart,
                                    const UTF8 *sourceEnd, UTF32 **targetStart,
                                    UTF32 *targetEnd, ConversionFlags flags) {
  return ConvertUTF8toUTF32Impl(sourceStart, sourceEnd, targetStart, targetEnd,
                                flags, /*InputIsPartial=*/false);
}

/* ---------------------------------------------------------------------

    Note A.
    The fall-through switches in UTF-8 reading code save a
    temp variable, some decrements & conditionals.  The switches
    are equivalent to the following loop:
        {
            int tmpBytesToRead = extraBytesToRead+1;
            do {
                ch += *source++;
                --tmpBytesToRead;
                if (tmpBytesToRead) ch <<= 6;
            } while (tmpBytesToRead > 0);
        }
    In UTF-8 writing code, the switches on "bytesToWrite" are
    similarly unrolled loops.

   --------------------------------------------------------------------- */

   #end