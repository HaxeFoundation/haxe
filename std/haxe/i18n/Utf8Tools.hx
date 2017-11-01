package haxe.i18n;

import haxe.i18n.Utf8.Utf8Impl;
import haxe.io.Bytes;

@:allow(haxe.i18n)
class Utf8Tools {

	// implementation specific

	static inline function fastGet (ba:Utf8Impl, pos:Int) {
		return ba.b.fastGet(pos);
	}

	static inline function set (ba:Utf8Impl, pos:Int, val:Int) {
		return ba.b.set(pos, val);
	}

	static inline function byteLength (ba:Utf8Impl) {
		return ba.b.length;
	}

	static inline function strLength (ba:Utf8Impl) {
		return ba.length;
	}

	static inline function mkImplFromBuffer(buf:ByteAccessBuffer, newSize:Int) {
		return {
			b : buf.getByteAccess(),
			length : newSize
		}
	}

	static inline function sub (ba:Utf8Impl, pos:Int, size:Int, newLen:Int):Utf8Impl {
		var bytes = ba.b.sub(pos, size);
		return {
			b : bytes,
			length : newLen
		}
	}

	static inline function allocImpl (size:Int, strLength:Int) {
		return {
			b : ByteAccess.alloc(size),
			length : strLength
		}
	}

	static function nativeStringToImpl (s:String):Utf8Impl {
		var ba = nativeStringToByteAccess(s);
		return {
			b : ba,
			length : calcLength(ba)
		}
	}

	static inline function append (ba:Utf8Impl, other:Utf8Impl):Utf8Impl {
		return {
			length : ba.length + other.length,
			b : ba.b.append(other.b)
		}
	}

	static inline function equal (ba:Utf8Impl, other:Utf8Impl):Bool {
		return ba.length == other.length && ba.b.equal(other.b);
	}

	static inline function toBytes(impl:Utf8Impl) : haxe.io.Bytes {
		return impl.b.copy().toBytes();
	}

	static function fromByteAccess (ba:ByteAccess):Utf8Impl {
		if (!Encoding.isLegalUtf8String(new Utf8Reader(ba))) {
			throw "illegal utf8";
		}
		var len = calcLength(ba);
		return { length : len, b : ba};
	}

	

	public static inline function toNativeString(impl:Utf8Impl) : String {
		return impl.b.getString(0, impl.length);
	}

	// end implementation specific

	// helper functions

	static function calcLength(ba:ByteAccess) {
		var len = 0;
		var index = 0;
		while (index < ba.length) {
			var size = getCharSize(ba.fastGet(index));
			len++;
			index += size;
		}
		return len;
	}

	static function getCharSize (start:Int):Int {
		return Encoding.getUtf8CharSize(start);
		
	}

	static function isUpperCaseLetter (bytes:Utf8Impl, pos:Int, size:Int) {
		var b = fastGet(bytes, pos);
		return b >= 0x41 && b <= 0x5A;
	}

	
	static function isLowerCaseLetter (bytes:Utf8Impl, pos:Int, size:Int) {
		var b = fastGet(bytes, pos);
		return b >= 0x61 && b <= 0x7A;
	}

	
	static function toLowerCaseLetter (bytes:Utf8Impl, target:Utf8Impl, pos:Int, size:Int) {
		if (isUpperCaseLetter(bytes, pos, size)) {
			set(target, pos, fastGet(bytes, pos)+0x20);
		} else {
			for (i in 0...size) {
				set(target, pos+i, fastGet(bytes, pos+i));
			}
		}
	}
	
	static function toUpperCaseLetter (bytes:Utf8Impl, target:Utf8Impl, pos:Int, size:Int) {
		if (isLowerCaseLetter(bytes, pos, size)) {
			set(target, pos, fastGet(bytes, pos)-0x20);
		} else {
			for (i in 0...size) {
				set(target, pos+i, fastGet(bytes, pos+i));
			}
		}
	}

	static var empty = allocImpl(0, 0);

	static inline function eachCode ( ba:Utf8Impl, f : Int -> Void) {
		var i = 0;
		while (i < byteLength(ba)) {
			var b = fastGet(ba, i);
			var size = getCharSize(b);
			var code = getCharCode(ba, i, size);
			f(code);
			i += size;
		}
	}


	static function getCharCode ( b:Utf8Impl, pos:Int, size:Int):Int {
		return switch size {
			case 1: fastGet(b, pos);
			case 2: (fastGet(b, pos) << 8) | (fastGet(b, pos+1));
			case 3: (fastGet(b, pos) << 16) | (fastGet(b, pos+1) << 8) | fastGet(b, pos+2);
			case _: throw "invalid byte sequence";
		}
	}

	static function compareChar ( b1:Utf8Impl, pos1:Int, b2:Utf8Impl, pos2:Int, size:Int):Int {
		var c1 = getCharCode(b1, pos1, size);
		var c2 = getCharCode(b2, pos2, size);

		return c1 - c2;
	}

	static function pushCharCode (bytes:Utf8Impl, buf:ByteAccessBuffer, pos:Int, size:Int) {
		for (i in 0...size) {
			buf.addByte(fastGet(bytes, pos+i));
		}
	}
	
	static function getCodeSize (code:Int):Int {
		return if (code <= 0x7F) {
			1;
		} else if (code <= 0x7FF) {
			2;
		} else if (code <= 0xFFFF) {
			3;
		} else if (code <= 0x10FFFF) {
			4;
		} else {
			throw "invalid code " + code;
		}
	}

	static function nativeStringToByteAccess (s:String):ByteAccess {
 		#if python
		// strings are utf-32
 		return ByteAccess.ofData(python.NativeStringTools.encode(s, "utf-8"));
 		#elseif (js || flash)
		return EncodingTools.ucs2ToUtf8ByteAccess( new Ucs2(s));
 		#else
		// strings are encoded as utf8 on other platforms
 		return ByteAccess.fromBytes(Bytes.ofString(s));
 		#end
 	}

	// string functions

	static function toUpperCase(ba:Utf8Impl) : Utf8Impl {
		var res = allocImpl(byteLength(ba), strLength(ba));
		var i = 0;
		while (i < byteLength(ba)) {
			var b = fastGet(ba, i);
			var size = getCharSize(b);
			toUpperCaseLetter(ba, res, i, size);

			i += size;
		}
		return res;
	}

	static function toLowerCase(ba:Utf8Impl) : Utf8Impl {
		var res = allocImpl(byteLength(ba), strLength(ba));
		var i = 0;
		while (i < byteLength(ba)) {
			var b = fastGet(ba, i);
			var size = getCharSize(b);
			toLowerCaseLetter(ba, res, i, size);

			i += size;
		}
		return res;
	}
	
	static function charAt(ba:Utf8Impl, index : Int) : Utf8Impl {
		var res = null;
		var pos = 0;
		var i = 0;
		while (i < byteLength(ba)) {
			var b = fastGet(ba, i);
			var size = getCharSize(b);
			if (pos == index) {
				res = sub(ba, i, size, 1);
				break;
			}

			pos++;
			i += size;
		}
		return res == null ? empty : res;
	}

	static function charCodeAt( ba:Utf8Impl, index : Int) : Null<Int> {
		var pos = 0;
		var i = 0;
		var r:Null<Int> = null;
		while (r == null && i < byteLength(ba)) {

			var b = fastGet(ba, i);
			var size = getCharSize(b);
			if (pos == index) {
				r = getCharCode(ba, i, size);
			} else {
				pos++;
				i += size;
			}
		}
		return r;
	}

	static function indexOf( ba:Utf8Impl, str : Utf8Impl, ?startIndex : Int ) : Int
	{
		var strLen = strLength(str);
		var res = -1;
		var len = strLen; // O(n)
		var pos = 0;
		var posFull = 0;
		var byteLength = byteLength(ba);
		// byte iteration variables
		var i = 0;
		var j = 0;

		var startIndex = startIndex == null ? 0 : startIndex;

		if (startIndex > 0) {
			while (i < byteLength) {
				var size = getCharSize(fastGet(ba, i));
				i+=size;
				posFull++;
				if (posFull >= startIndex) break;
			}
		}
		// iterate bytes
		while (i < byteLength) {
			var size = getCharSize(fastGet(ba, i));

			if (compareChar(ba, i, str, j, size) == 0) {
				pos++;
				j+=size;
			} else {
				j = 0;
				pos = 0;
			}

			i+=size;
			if (pos == len) {
				res = posFull;
				break;
			}
			if (pos == 0) {
				posFull++;
			}
		}
		return res;
	}

	@:analyzer(no_code_motion) static function lastIndexOf( ba:Utf8Impl, str : Utf8Impl, ?startIndex : Int ) : Int {
		var startIndexIsNull = startIndex == null;
		var res = -1;
		var len = strLength(str); // O(n)
		var pos = 0;
		var posFull = 0;
		
		// byte iteration variables
		var i = 0;
		var j = 0;
		
		var iNext = 0;
		
		while (i < byteLength(ba) && (startIndexIsNull || posFull < startIndex + 1)) {
			var size = getCharSize(fastGet(ba, i));
			if (compareChar(ba, i, str, j, size) == 0) {
				if (j == 0) {
					// store the next position for next search
					iNext = i + size;
				}
				pos++;
				j+=size;
				
			} else {
				j = 0;
				pos = 0;
			}

			i+=size;
			if (pos == len) {
				res = posFull;
				posFull++;
				i = iNext; // restore search position for next search
				j = 0;
				pos = 0;
				continue;
			}
			if (pos == 0) {
				posFull++;
			}
		}
		return res;
	}

	@:access(haxe.i18n.Utf8.fromImpl)
	static inline function split( ba:Utf8Impl, delimiter : Utf8Impl ) : Array<Utf8>
	{
		var delimiterLen = strLength(delimiter);
		var buf = new ByteAccessBuffer();
		var tmpBuf = new ByteAccessBuffer();
		var bufLen = 0; // store utf8 len
		var tmpBufLen = 0; // store utf8 len
		var res:Array<Utf8> = [];
		var len = delimiterLen; // O(n)
		var str = delimiter;
		var pos = 0;
		var posFull = 0;
		var byteLength = byteLength(ba);
		// byte iteration variables
		var i = 0;
		var j = 0;
		// iterate bytes
		while (i < byteLength) {
			var size = getCharSize(fastGet(ba, i));

			if (compareChar(ba, i, str, j, size) == 0) {

				pos++;
				j+=size;
				tmpBufLen++;
				for (k in 0...size) {
					tmpBuf.addByte(fastGet(ba, i+k));
				}
			} else {
				if (pos != 0) {
					j = 0;
					pos = 0;
					buf.addBuffer(tmpBuf);
					bufLen += tmpBufLen;
					tmpBufLen = 0;
					tmpBuf.reset();
				}
				for (k in 0...size) {
					buf.addByte(fastGet(ba, i+k));
					bufLen++;
				}
			}
			i+=size;
			if (pos == len) {
				if (buf.length > 0) {
					res.push(Utf8.fromImpl(mkImplFromBuffer(buf, bufLen)));
					bufLen = 0;
					buf.reset();
				} else {
					res.push(Utf8.fromImpl(empty));
				}
				tmpBuf.reset();
				tmpBufLen = 0;
				j = 0;
				pos = 0;
			}
			posFull++;
		}
		if (pos != 0) {
			j = 0;
			pos = 0;
			buf.addBuffer(tmpBuf);
			bufLen += tmpBufLen;
		}
		if (buf.length > 0) {
			res.push(Utf8.fromImpl(mkImplFromBuffer(buf, bufLen)));
		} else {
			res.push(Utf8.fromImpl(empty));
		}
		return res;
	}
	
	@:analyzer(no_code_motion) // see https://github.com/HaxeFoundation/haxe/issues/5826
	static function substr<T>( ba:Utf8Impl, pos : Int, ?len : Int ) : Utf8Impl {

		var lenIsNull = len == null;
		if (pos < 0) {
			var thisLength = byteLength(ba);
			pos = thisLength + pos;
			if (pos < 0) pos = 0;
		}

		if (!lenIsNull && len < 0) {
			len = byteLength(ba) + len;
			if (len < 0) len = 0;
		}

		if (len == 0) return empty;

		var buf = new ByteAccessBuffer();

		var str = ba;

		var cur = 0;

		var byteLength = byteLength(str);
		var i = 0;
		var newSize = 0;
		while (i < byteLength) {
			var char = fastGet(str, i);
			//if (char == null) throw "error";
			var size = getCharSize(char);
			if (cur >= pos && (lenIsNull || cur < pos + len))
			{
				newSize++;
				pushCharCode(str, buf, i, size);
			} else if (!lenIsNull && cur >= pos+len) {
				break;
			}

			i+=size;
			cur++;
		}
		return mkImplFromBuffer(buf, newSize);
	}

	@:analyzer(no_code_motion) static inline function substring<T>( ba:Utf8Impl, startIndex : Int, ?endIndex : Int ) : Utf8Impl {
		var len = strLength(ba);
		var endIndexIsNull = endIndex == null; 
		var startIndex:Null<Int> = startIndex;
		if (startIndex < 0) startIndex = 0;
		if (!endIndexIsNull && endIndex < 0) endIndex = 0;
		
		if (endIndexIsNull) endIndex = len;
 		if (startIndex > endIndex) {
			var x = startIndex;
			startIndex = endIndex;
			endIndex = x;
		}

		if (endIndex == null || endIndex > len) endIndex = len;

		if (startIndex == null || startIndex > len) return empty;
		
		return substr(ba, startIndex, endIndex - startIndex);
	}


	static function fromCharCode( code : Int ) : Utf8Impl
	{
		var size = getCodeSize(code);
		var bytes = allocImpl(size, 1);
		switch size {
			case 1:
				set(bytes, 0, code);
			case 2:
				set(bytes, 0, 0xC0 | (code >> 6));
				set(bytes, 1, 0x80 | (code & 0x3F));
			case 3:
				set(bytes, 0, 0xE0 | (code >> 12));
				set(bytes, 1, 0x80 | ((code >> 6) & 0x3F));
				set(bytes, 2, 0x80 | (code & 0x3F));
			case 4:
				set(bytes, 0, 0xF0 | (code >> 18));
				set(bytes, 1, 0x80 | ((code >> 12)  & 0x3F));
				set(bytes, 2, 0x80 | ((code >> 6) & 0x3F));
				set(bytes, 3, 0x80 | (code & 0x3F));
			case _: throw "invalid char code";
		}
		return bytes;
	}


	static function toCodeArray (ba:Utf8Impl):Array<Int> {
		var res = [];
		eachCode(ba, function (c) res.push(c));
		return res;
	}


	

	 static function compare (ba:Utf8Impl, other:Utf8Impl):Int {
		var len1 = strLength(ba);
		var len2 = strLength(other);
		var min = len1 < len2 ? len1 : len2;
		for (i in 0...min) {
			var a = charCodeAt(ba, i);
			var b = charCodeAt(other, i);
			if (a < b) return -1;
			if (a > b) return 1;
		}
		if (len1 < len2) return -1;
		if (len1 > len2) return 1;
		return 0;
	}


	

}