package haxe.i18n;

import haxe.i18n.Utf8;

import haxe.io.Bytes;
import haxe.i18n.ByteAccessBuffer;

import haxe.i18n.Encoding;


// check http://llvm.org/svn/llvm-project/llvm/trunk/lib/Support/ConvertUTF.c

class EncodingTools {

	/*
	

	public static inline var minLowSurrogate : Int = 0xDC00;
	public static inline var maxLowSurrogate : Int = 0xDFFF;

	public static inline var minCodePoint : Int = 0x0000;
	public static inline var maxCodePoint : Int = 0x10FFFF;

	

	public static inline function isLowSurrogate(code : Int) : Bool {
		return minLowSurrogate <= code && code <= maxLowSurrogate;
	}

	public static inline function isScalar(code : Int) : Bool {
		return minCodePoint <= code && code <= maxCodePoint && !isHighSurrogate(code) && !isLowSurrogate(code);
	}
	*/

	public static inline var minHighSurrogate : Int = 0xD800;
	public static inline var maxHighSurrogate : Int = 0xDBFF;

	public static inline function isHighSurrogate(code : Int) : Bool {
		return minHighSurrogate <= code && code <= maxHighSurrogate;
	}

	
	public static function ucs2ToUtf16 (s:Ucs2):Utf16 {
		// every ucs2 character is a valid utf16 character
		// TODO: this could be a no op, we can reuse the same bytes
		return Utf16.fromBytes(s.toBytes());
	}
	

	/*
	 *	Can throw surrogate errors
	*/
	
	
	public static function utf16ToUcs2 (s:Utf16):Ucs2 {
		return Ucs2.fromBytes(s.toBytes()); 
	}
	
	public static function ucs2ToUtf8 (s:Ucs2):Utf8 {
		return Utf8.fromBytes(ucs2ToUtf8ByteAccess(s).toBytes());
	}

	
	public static inline function utf8ToUcs2 (s:Utf8):Ucs2 {
		// TODO this could be done without creating Bytes via fromByteAccess
		return Ucs2.fromBytes(utf8ToUcs2ByteAccess(s).toBytes());
	}

	public static inline function utf16ToUtf8 (s:Utf16):Utf8 {
		return Utf8.fromByteAccess(Encoding.convertUTF16toUTF8(Utf16.asByteAccess(s), StrictConversion));
	}

	public static inline function utf8ToUtf16 (s:Utf8):Utf16 {
		return Utf16.fromByteAccess(Encoding.convertUTF8toUTF16(s.getByteReader(), StrictConversion));
	}

	public static function ucs2ToUtf8ByteAccess (s:Ucs2):ByteAccess {
		var b = new ByteAccessBuffer();

		for (i in 0...s.length) {
			var c = s.charCodeAt(i);
			charUcs2ToUtf8(c, b);
		}
		var bytes = b.getByteAccess();

		return bytes;
	}

	public static inline function getUtf16CodeSize (code:Int):Int {
		return if (code <= 0xFFFF) 2 else 4;
	}

	public static function charCodeToUtf16ByteAccess (code:Int):ByteAccess {
		var size = getUtf16CodeSize(code);
		var bytes = ByteAccess.alloc(size);
		switch size {
			case 2:
				bytes.set(0, (code & 0xFF00) >> 8);
				bytes.set(1, code & 0x00FF);
			case 4:
				var c1 = (code >> 10) + 0xD7C0;
				var c2 = (code & 0x3FF) | 0xDC00;
				bytes.set(0, (c1 & 0xFF00) >> 8);
				bytes.set(1, (c1 & 0xFF));
				bytes.set(2, (c2 & 0xFF00) >> 8);
				bytes.set(3, (c2 & 0xFF));
			case _: throw "invalid char code";
		}
		return bytes;
	}

	public static function charCodeToUcs2ByteAccess (code:Int):ByteAccess {
		var size = getUtf16CodeSize(code);
		var bytes = ByteAccess.alloc(size);
		switch size {
			case 2:
				bytes.set(0, (code & 0xFF00) >> 8);
				bytes.set(1, code & 0x00FF);
			case 4:
				throw "no surrogate pairs allowed";

			case _: throw "invalid char code";
		}
		return bytes;
	}

	public static inline function utf8ToUcs2ByteAccess (s:Utf8):ByteAccess {
		return utf8ByteAccessToUcs2ByteAccess(s.getByteReader());
	}

	public static function utf8ByteAccessToUcs2ByteAccess (b:ByteReader):ByteAccess {
		var buf = new ByteAccessBuffer();

		var index = 0;
		var max = b.length;

		while (index < max) {

			var read = charUtf8ToUcs2(b, index, buf);
			index += read;
		}

		var bytes = buf.getByteAccess();

		return bytes;
	}

	

	/*
	  Converts the ucs-2 charater `ucs2` into utf-8 bytes which are written
	  into the buffer `buf`.
	*/
	static function charUcs2ToUtf8 (ucs2:Int, buf:ByteAccessBuffer):Void
	{
	    if (ucs2 < 0x80) {
	    	buf.addByte(ucs2);
	    }
	    else if (ucs2 >= 0x80  && ucs2 < 0x800) {
	    	buf.addByte((ucs2 >> 6)   | 0xC0);
	    	buf.addByte((ucs2 & 0x3F) | 0x80);
	    }
	    else if (ucs2 >= 0x800 && ucs2 < 0xFFFF) {
			if (ucs2 >= 0xD800 && ucs2 <= 0xDFFF) {
			    /* Ill-formed. */
			    trace(StringTools.hex(ucs2));
			    throw UnicodeError.SurrogatePair;
			}
			buf.addByte(((ucs2 >> 12)       ) | 0xE0);
			buf.addByte(((ucs2 >> 6 ) & 0x3F) | 0x80);
			buf.addByte(((ucs2      ) & 0x3F) | 0x80);
	    }
	    else if (ucs2 >= 0x10000 && ucs2 < 0x10FFFF) {
			/* http://tidy.sourceforge.net/cgi-bin/lxr/source/src/utf8.c#L380 */
			buf.addByte(0xF0 | (ucs2 >> 18));
			buf.addByte(0x80 | ((ucs2 >> 12) & 0x3F));
			buf.addByte(0x80 | ((ucs2 >> 6) & 0x3F));
			buf.addByte(0x80 | ((ucs2 & 0x3F)));
	    }
	    else throw UnicodeError.BadUcs2Input;
	}

    /*
    	Convert the UTF-8 encoded character starting at position `start` in `input` and
       	pushes the generated ucs-2 bytes into the buffer `buf`.
       	It returns the number of bytes which the utf-8 character takes.
	*/
	static function charUtf8ToUcs2 (input:ByteReader, start:Int, buf:ByteAccessBuffer):Int {
		var first = input.fastGet(start); 
	    if (first < 0x80) {
			/* One byte (ASCII) case. */
			buf.addByte(0);
			buf.addByte( first );

			return 1;
	    }
	    if ((first & 0xE0) == 0xE0) {
			/* Three byte case. */
	        if (input.fastGet(start+1) < 0x80 || input.fastGet(start+1) > 0xBF ||
		    	input.fastGet(start+2) < 0x80 || input.fastGet(start+2) > 0xBF) {
	            return throw UnicodeError.BadUtf8Input;
			}

			var val = ((first & 0x0F) << 12) | ((input.fastGet(start+1) & 0x3F) << 6) | (input.fastGet(start+2) & 0x3F);
			buf.addByte( (val & 0xFF00) >> 8 );
			buf.addByte( val & 0x00FF );
	        return 3;
	    }
	    if ((first & 0xC0) == 0xC0) {
			/* Two byte case. */
	        if (input.fastGet(start+1) < 0x80 || input.fastGet(start+1) > 0xBF) {
	            return throw UnicodeError.BadUtf8Input;
			}
			var val = ((first & 0x1F)<<6) | (input.fastGet(start+1) & 0x3F);
			buf.addByte( (val & 0xFF00) >> 8 );
			buf.addByte( (val & 0x00FF) );
			return 2;
	    }
	    return throw UnicodeError.BadUtf8Input;
	}

	/*
	public static function utf8Iterator (s:Utf8, beginIndex:Int, endIndex:Int ):Iterator<CodePoint> {

	}

	public static function ucs2Iterator (s:Ucs2, beginIndex:Int, endIndex:Int):Iterator<CodePoint> {

	}

	public static function utf16Iterator (s:Utf16, beginIndex:Int, endIndex:Int):Iterator<CodePoint> {

	}

	public static function utf32Iterator (s:Utf32, beginIndex:Int, endIndex:Int):Iterator<CodePoint> {

	}
	*/

}