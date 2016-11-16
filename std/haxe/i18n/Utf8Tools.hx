package haxe.i18n;



@:allow(haxe.i18n)
class Utf8Tools {

	static inline function fastGet (ba:ByteAccess, pos:Int) {
		return ba.fastGet(pos);
	}

	static inline function set (ba:ByteAccess, pos:Int, val:Int) {
		return ba.set(pos, val);
	}

	static inline function byteLength (ba:ByteAccess) {
		return ba.length;
	}

	static inline function sub (ba:ByteAccess, pos:Int, size:Int, newLen:Int) {
		return ba.sub(pos, size);
	}

	static inline function alloc (size:Int, strLength:Int) {
		return ByteAccess.alloc(size);
	}

	static function getLength(ba:ByteAccess) {
		var len = 0;
		var index = 0;
		while (index < ba.length) {
			var size = getCharSize(fastGet(ba, index));
			len++;
			index += size;
		}
		return len;
	}

	static function getCharSize (start:Int):Int {
		return if (start < 0x80) 1
		else if ((start & 0xE0) == 0xE0) 3
		else if ((start & 0xC0) == 0xC0) 2
		else throw "invalid utf8";
	}

	
	static function toUpperCase(ba:ByteAccess, len:Int) : ByteAccess {
		var res = alloc(byteLength(ba), len);
		var i = 0;
		while (i < byteLength(ba)) {
			var b = fastGet(ba, i);
			var size = getCharSize(b);
			toUpperCaseLetter(ba, res, i, size);

			i += size;
		}
		return res;
	}


	
	static function isUpperCaseLetter (bytes:ByteAccess, pos:Int, size:Int) {
		var b = fastGet(bytes, pos);
		return b >= 0x41 && b <= 0x5A;
	}

	
	static function isLowerCaseLetter (bytes:ByteAccess, pos:Int, size:Int) {
		var b = fastGet(bytes, pos);
		return b >= 0x61 && b <= 0x7A;
	}

	
	static function toLowerCaseLetter (bytes:ByteAccess, target:ByteAccess, pos:Int, size:Int) {
		if (isUpperCaseLetter(bytes, pos, size)) {
			set(target, pos, fastGet(bytes, pos)+0x20);
		} else {
			for (i in 0...size) {
				set(target, pos+i, fastGet(bytes, pos+i));
			}
		}
	}

	
	static function toUpperCaseLetter (bytes:ByteAccess, target:ByteAccess, pos:Int, size:Int) {
		if (isLowerCaseLetter(bytes, pos, size)) {
			set(target, pos, fastGet(bytes, pos)-0x20);
		} else {
			for (i in 0...size) {
				set(target, pos+i, fastGet(bytes, pos+i));
			}
		}
	}

	
	static function toLowerCase(ba:ByteAccess, len:Int) : ByteAccess {
		var res = alloc(byteLength(ba), len);
		var i = 0;
		while (i < byteLength(ba)) {
			var b = fastGet(ba, i);
			var size = getCharSize(b);
			toLowerCaseLetter(ba, res, i, size);

			i += size;
		}
		return res;
	}

	
	static function charAt(ba:ByteAccess, index : Int) : ByteAccess {
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
		return res == null ? empty() : res;
	}

	static var emptyByteAccess = ByteAccess.alloc(0);

	static function empty () {
		return emptyByteAccess;
	}

	
	static inline function eachCode ( ba:ByteAccess, f : Int -> Void) {
		var i = 0;
		while (i < byteLength(ba)) {
			var b = fastGet(ba, i);
			var size = getCharSize(b);
			var code = getCharCode(ba, i, size);
			f(code);
			i += size;
		}
	}


	static function getCharCode ( b:ByteAccess, pos:Int, size:Int):Int {
		return switch size {
			case 1: fastGet(b, pos);
			case 2: (fastGet(b, pos) << 8) | (fastGet(b, pos+1));
			case 3: (fastGet(b, pos) << 16) | (fastGet(b, pos+1) << 8) | fastGet(b, pos+2);
			case _: throw "invalid byte sequence";
		}
	}
	
	static function charCodeAt( ba:ByteAccess, index : Int) : Null<Int> {
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

	
	static function indexOf( ba:ByteAccess, str : ByteAccess, strLen:Int, ?startIndex : Int ) : Int
	{

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


	static function compareChar ( b1:ByteAccess, pos1:Int, b2:ByteAccess, pos2:Int, size:Int):Int {
		var c1 = getCharCode(b1, pos1, size);
		var c2 = getCharCode(b2, pos2, size);

		return c1 - c2;
	}
	

	@:analyzer(no_code_motion) static function lastIndexOf( ba:ByteAccess, str : ByteAccess, strLen:Int, ?startIndex : Int ) : Int {
		var startIndexIsNull = startIndex == null;
		var other = str;
		var res = -1;
		var len = strLen; // O(n)
		var pos = 0;
		var posFull = 0;
		var byteLength = byteLength(ba);
		
		// byte iteration variables
		var i = 0;
		var j = 0;
		
		var iNext = 0;
		
		while (i < byteLength && (startIndexIsNull || posFull < startIndex + 1)) {
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

	static inline function split<T>( ba:ByteAccess, delimiter : ByteAccess, delimiterLen:Int, mkElement:ByteAccess->Int->T ) : Array<T>
	{
		var buf = new ByteAccessBuffer();
		var tmpBuf = new ByteAccessBuffer();

		var res:Array<T> = [];
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
				for (k in 0...size) {
					tmpBuf.addByte(fastGet(ba, i+k));
				}
			} else {
				if (pos != 0) {
					j = 0;
					pos = 0;
					buf.addBuffer(tmpBuf);
					tmpBuf.reset();
				}
				for (k in 0...size) {
					buf.addByte(fastGet(ba, i+k));
				}
			}
			i+=size;
			if (pos == len) {
				if (buf.length > 0) {
					var ba = buf.getByteAccess();
					var len = getLength(ba);
					res.push(mkElement(ba, len));
					buf.reset();
				} else {
					res.push(mkElement(empty(), 0));
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
			var ba = buf.getByteAccess();
			var len = getLength(ba);
			res.push(mkElement(ba, len));
		} else {
			res.push(mkElement(empty(), 0));
		}
		return res;
	}
	
	@:analyzer(no_code_motion) static inline function substr<T>( ba:ByteAccess, pos : Int, mkElement:ByteAccess->Int->T, ?len : Int ) : T {

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

		if (len == 0) return mkElement(empty(), 0);

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
		return mkElement(buf.getByteAccess(), newSize);
	}

	
	static function pushCharCode (bytes:ByteAccess, buf:ByteAccessBuffer, pos:Int, size:Int) {
		for (i in 0...size) {
			buf.addByte(fastGet(bytes, pos+i));
		}
	}

	@:analyzer(no_code_motion) static inline function substring<T>( ba:ByteAccess, len:Int, startIndex : Int, mkElement:ByteAccess->Int->T, ?endIndex : Int ) : T {
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

		if (startIndex == null || startIndex > len) return mkElement(empty(), 0);
		
		return substr(ba, startIndex, mkElement, endIndex - startIndex);
	}


	static function fromCharCode( code : Int ) : ByteAccess
	{
		var size = getCodeSize(code);
		var bytes = alloc(size, 1);
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


	static function toCodeArray (ba:ByteAccess):Array<Int> {
		var res = [];
		eachCode(ba, function (c) res.push(c));
		return res;
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
 		return ByteAccess.ofData(python.NativeStringTools.encode(s, "utf-8"));
 		#elseif (js || flash)
		return EncodingTools.ucs2ToUtf8ByteAccess( new Ucs2(s));
 		#else
 		return ByteAccess.fromBytes(Bytes.ofString(s));
 		#end
 	}

	 static function compare (ba:ByteAccess, baLen:Int, other:ByteAccess, otherLen:Int):Int {
		var len1 = baLen;
		var len2 = otherLen;
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

	/*
	
	inline function getByteLength() {
		return this.length;
	}

	// private helpers

	
	public static function asByteAccess (s:Utf8):ByteAccess {
		return cast s;
	}

	
	public static function fromByteAccess (bytes:ByteAccess):Utf8 {
		return cast bytes;
	}

	

	public static function fromBytes( bytes : haxe.io.Bytes ) : Utf8 {
		return fromByteAccess(ByteAccess.fromBytes(bytes).copy());
	}


	
	public static function fromNativeString (s:String):Utf8 {
		return fromByteAccess(nativeStringToByteAccess(s));
 	}

 	
	public function toNativeString() : String {
		return this.getString(0, this.length);
	}

 	
	public function toUcs2() : Ucs2 {
		return EncodingTools.utf8ToUcs2(fromByteAccess(this));
	}

	
 	public function toUtf16 ():Utf16 {
		return EncodingTools.utf8ToUtf16(fromByteAccess(this));
	}

	
	public function toBytes() : haxe.io.Bytes {
		return this.copy().toBytes();
	}

	@:op(A + B) inline function opAdd (other:Utf8) {
		return fromByteAccess(this.append(asByteAccess(other)));
	}

	@:op(A == B) public function opEq (other:Utf8) {
		return this.equal(asByteAccess(other));
	}

	@:op(A != B) inline function opNotEq (other:Utf8) {
		return !opEq(other);
	}

	

	@:op(A > B) inline function opGreaterThan (other:Utf8) {
		return compare(other) == 1;
	}
	@:op(A < B) inline function opLessThan (other:Utf8) {
		return compare(other) == -1;
	}
	@:op(A <= B) inline function opLessThanOrEq (other:Utf8) {
		return compare(other) <= 0;
	}

	@:op(A >= B) inline function opGreaterThanOrEq (other:Utf8) {
		return compare(other) >= 0;
	}
	*/
}