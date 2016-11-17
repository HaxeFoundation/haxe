package haxe.i18n;

import haxe.i18n.Utf16.Utf16Impl;

class Utf16Tools {


	static inline function byteLength (ba:Utf16Impl) {
		return ba.length;
	}

	static inline function fastGet (ba:Utf16Impl, pos:Int) {
		return ba.fastGet(pos);
	}

	static function nativeStringToImpl (s:String):Utf16Impl {
		var ba = nativeStringToByteAccess(s);
		return ba;
	}

	public static function toUpperCase(impl:Utf16Impl) : Utf16Impl {
		var res = ByteAccess.alloc(impl.length);
		var i = 0;
		while (i < impl.length) {
			var b = getNext2Bytes(impl, i);
			var size = getCharSize(b);
			toUpperCaseLetter(impl, res, i, size);
			i += size;
		}
		return res;
	}
	public static function toLowerCase(impl:Utf16Impl) : Utf16Impl {
		var res = ByteAccess.alloc(impl.length);
		var i = 0;
		while (i < impl.length) {
			var b = getNext2Bytes(impl, i);
			var size = getCharSize(b);
			toLowerCaseLetter(impl, res, i, size);

			i += size;
		}
		return res;
	}

	static inline function getNext2Bytes (impl:Utf16Impl, pos:Int) {
		var b1 = impl.fastGet(pos);
		var b2 = impl.fastGet(pos+1);
		return (b1 << 8) | b2;
	}

	public static function charAt(impl:Utf16Impl, index : Int) : Utf16Impl {
		var res = null;
		var pos = 0;
		var i = 0;
		while (i < impl.length) {
			var b = getNext2Bytes(impl, i);
			var size = getCharSize(b);
			if (pos == index) {
				res = impl.sub(i, size);
				break;
			}

			pos++;
			i += size;
		}
		return res == null ? empty : res;
	}

	public static function toCodeArray (impl:Utf16Impl) {
		var res = [];
		eachCode(impl, function (c) res.push(c));
		return res;
	}

	public static function charCodeAt( impl:Utf16Impl, index : Int) : Null<Int> {
		var len = impl.length;
		if (index < 0 || index >= len) return null;

		var pos = 0;
		var i = 0;
		while (i < len) {

			var b = impl.fastGet(i);
			var size = getCharSize(b);
			if (pos == index) {
				return getCharCode(impl, i, size);
			} else {
				pos++;
				i += size;
			}
		}
		return null;
	}


	static inline function eachCode ( impl:Utf16Impl, f : Int -> Void) {
		var i = 0;
		while (i < impl.length) {
			var b = impl.fastGet(i);
			var size = getCharSize(b);
			var code = getCharCode(impl, i, size);
			f(code);
			i += size;
		}
	}

	public static function indexOf( impl:Utf16Impl, str : Utf16Impl, ?startIndex : Int ) : Int
	{

		var res = -1;
		var len = strLength(str); // O(n)
		var pos = 0;
		var posFull = 0;
		var byteLength = impl.length;
		// byte iteration variables
		var i = 0;
		var j = 0;

		var startIndex = startIndex == null ? 0 : startIndex;

		if (startIndex > 0) {
			while (i < byteLength) {
				var size = getCharSize(impl.fastGet(i));
				i+=size;
				posFull++;
				if (posFull >= startIndex) break;
			}
		}
		// iterate bytes
		while (i < byteLength) {
			var size = getCharSize(impl.fastGet(i));

			if (compareChar(impl, i, str, j, size) == 0) {
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
	
	@:analyzer(no_code_motion) static function lastIndexOf( ba:Utf16Impl, str : Utf16Impl, ?startIndex : Int ) : Int {
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
	
	public static function split( impl:Utf16Impl, delimiter : Utf16Impl ) : Array<Utf16>
	{
		var buf = new ByteAccessBuffer();
		var tmpBuf = new ByteAccessBuffer();

		var res:Array<Utf16> = [];
		var len = strLength(delimiter);
		var str = delimiter;
		var pos = 0;
		var posFull = 0;
		var byteLength = impl.length;
		// byte iteration variables
		var i = 0;
		var j = 0;
		// iterate bytes
		while (i < byteLength) {
			var size = getCharSize(impl.fastGet(i));

			if (compareChar(impl, i, str, j, size) == 0) {

				pos++;
				j+=size;
				for (k in 0...size) {
					tmpBuf.addByte(impl.fastGet(i+k));
				}
			} else {
				if (pos != 0) {
					j = 0;
					pos = 0;
					buf.addBuffer(tmpBuf);
					tmpBuf.reset();
				}
				for (k in 0...size) {
					buf.addByte(impl.fastGet(i+k));
				}
			}
			i+=size;
			if (pos == len) {
				if (buf.length > 0) {
					res.push(Utf16.fromImpl(buf.getByteAccess()));
					buf.reset();
				} else {
					res.push(Utf16.fromImpl(empty));
				}
				tmpBuf.reset();
				j = 0;
				pos = 0;
			}
			posFull++;
		}
		if (pos != 0) {
			j = 0;
			pos = 0;
			buf.addBuffer(tmpBuf);
		}
		if (buf.length > 0) {
			res.push(Utf16.fromImpl(buf.getByteAccess()));
		} else {
			res.push(Utf16.fromImpl(empty));
		}
		return res;
	}
	
	public static function substr( impl:Utf16Impl, pos : Int, ?len : Int ) : Utf16Impl {

		if (pos < 0) {
			var thisLength = strLength(impl);
			pos = thisLength + pos;
			if (pos < 0) pos = 0;
		}

		if (len != null && len < 0) {
			len = strLength(impl) + len;
			if (len < 0) len = 0;
		}

		if (len == 0) return empty;

		//trace("pos: " + pos, "len: " + len);



		var buf = new ByteAccessBuffer();

		var str = impl;

		var cur = 0;

		var byteLength = str.length;
		var i = 0;

		while (i < byteLength) {
			var char = getNext2Bytes(impl, i);
			//if (char == null) throw "error";
			var size = getCharSize(char);
			if (cur >= pos && (len == null || cur < pos + len))
			{
				pushCharCode(str, buf, i, size);
			} else if (len != null && cur >= pos+len) {
				break;
			}

			i+=size;
			cur++;
		}
		//trace(buf);
		return buf.getByteAccess();
	}

	public static function substring( impl:Utf16Impl, startIndex : Int, ?endIndex : Int ) : Utf16Impl {
		var startIndex:Null<Int> = startIndex;
		if (startIndex < 0) startIndex = 0;
		if (endIndex != null && endIndex < 0) endIndex = 0;
		
		var len = impl.length;

		if (endIndex == null) endIndex = len;

 		if (startIndex > endIndex) {
			var x = startIndex;
			startIndex = endIndex;
			endIndex = x;
		}

		if (endIndex == null || endIndex > len) endIndex = len;

		if (startIndex == null || startIndex > len) return empty;
		
		return substr(impl, startIndex, endIndex - startIndex);
	}

	static inline function equal (impl:Utf16Impl, other:Utf16Impl) {
		return impl.equal(other);
	}

	static inline function append (impl:Utf16Impl, other:Utf16Impl) {
		return impl.append(other);
	}

	// private helpers

	static function strLength(impl:Utf16Impl) {
		var len = 0;
		var index = 0;
		while (index < impl.length) {
			var size = getCharSize(getNext2Bytes(impl, index));

			len++;
			index += size;
		}
		return len;
	}

	static inline function getCharSize (start2Bytes:Int):Int {
		return if (EncodingTools.isHighSurrogate(start2Bytes)) 4 else 2;
	}

	static inline function isUpperCaseLetter (bytes:ByteAccess, pos:Int, size:Int) {
		var b1 = bytes.fastGet(pos);
		var b2 = bytes.fastGet(pos+1);
		return b1 == 0x00 && b2 >= 0x41 && b2 <= 0x5A;
	}

	static inline function isLowerCaseLetter (bytes:ByteAccess, pos:Int, size:Int) {
		var b1 = bytes.fastGet(pos);
		var b2 = bytes.fastGet(pos+1);
		return b1 == 0x00 && b2 >= 0x61 && b2 <= 0x7A;
	}

	static inline function toLowerCaseLetter (bytes:ByteAccess, target:ByteAccess, pos:Int, size:Int) {
		if (isUpperCaseLetter(bytes, pos, size)) {
			target.set(pos, bytes.fastGet(pos));
			target.set(pos+1, bytes.fastGet(pos+1)+0x20);
		} else {
			for (i in 0...size) {
				target.set(pos+i, bytes.fastGet(pos+i));
			}
		}
	}

	static inline function toUpperCaseLetter (bytes:ByteAccess, target:ByteAccess, pos:Int, size:Int) {
		if (isLowerCaseLetter(bytes, pos, size)) {
			target.set(pos, bytes.fastGet(pos));
			target.set(pos+1, bytes.fastGet(pos+1)-0x20);
		} else {
			for (i in 0...size) {
				target.set(pos+i, bytes.fastGet(pos+i));
			}
		}
	}

	static var empty = ByteAccess.alloc(0);

	static inline function getCharCode ( b:ByteAccess, pos:Int, size:Int):Int {
		return switch size {

			case 2: b.getInt16(pos);
			case 4: b.getInt32(pos);
			case _: throw "invalid byte sequence";
		}
	}

	static inline function compareChar ( b1:ByteAccess, pos1:Int, b2:ByteAccess, pos2:Int, size:Int):Int {
		var c1 = getCharCode(b1, pos1, size);
		var c2 = getCharCode(b2, pos2, size);

		return c1 - c2;
	}

	static inline function pushCharCode (bytes:ByteAccess, buf:ByteAccessBuffer, pos:Int, size:Int) {
		for (i in 0...size) {
			buf.addByte(bytes.fastGet(pos+i));
		}
	}

	public static function fromCharCode( code : Int ) : Utf16Impl
	{
		return EncodingTools.charCodeToUtf16ByteAccess(code);
	}

	public static function fromBytes( bytes : haxe.io.Bytes ) : Utf16Impl {
		return ByteAccess.fromBytes(bytes).copy();
	}

	public static function nativeStringToByteAccess (s:String):ByteAccess {
 		#if python
 		return ByteAccess.ofData(python.NativeStringTools.encode(s, "utf-16be"));
		#elseif (neko || cpp || php)
		return EncodingTools.utf8ToUtf16(new Utf8(s)).impl();
 		#elseif (js || flash || hl)
 		return EncodingTools.ucs2ToUtf16( new Ucs2(s)).impl();
 		#else
		var utf8Bytes = haxe.io.Bytes.ofString(s);
		return Utf8.fromByteAccess(ByteAccess.fromBytes(utf8Bytes)).toUtf16().impl();
 		#end
 	}

 	public static function toNativeString(impl:Utf16Impl) : String {
		return impl.getString(0, impl.length);
	}

	public static function toBytes(impl:Utf16Impl) : haxe.io.Bytes {
		return impl.copy().toBytes();
	}

	static function compare (impl:Utf16Impl, other:Utf16Impl):Int {
		var len1 = impl.length;
		var len2 = other.length;
		var min = len1 < len2 ? len1 : len2;
		for (i in 0...min) {
			var a = charCodeAt(impl, i);
			var b = charCodeAt(other, i);
			if (a < b) return -1;
			if (a > b) return 1;
		}
		if (len1 < len2) return -1;
		if (len1 > len2) return 1;
		return 0;
	}

}
