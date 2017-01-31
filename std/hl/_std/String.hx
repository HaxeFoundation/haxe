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
@:coreApi
class String {

	var bytes : hl.Bytes;
	public var length(default,null) : Int;

	public function new(string:String) : Void {
		bytes = string.bytes;
		length = string.length;
	}

	public function toUpperCase() : String {
		return __alloc__(@:privateAccess bytes.ucs2Upper(0,length), length);
	}

	public function toLowerCase() : String {
		return __alloc__(@:privateAccess bytes.ucs2Lower(0,length), length);
	}

	public function charAt(index : Int) : String {
		if( (index:UInt) >= (length:UInt) ) return "";
		var b = new hl.Bytes(4);
		b.setUI16(0, bytes.getUI16(index<<1));
		b.setUI16(2,0);
		return __alloc__(b,1);
	}

	public function charCodeAt( index : Int) : Null<Int> {
		var idx : UInt = index;
		if( idx >= (length:UInt) )
			return null;
		return bytes.getUI16(index << 1);
	}

	public function indexOf( str : String, ?startIndex : Int ) : Int {
		var startByte = 0;
		if( startIndex != null && startIndex > 0 ) {
			if( startIndex >= length )
				return -1;
			startByte = startIndex << 1;
		}
		var p = bytes.find(startByte, (length << 1) - startByte, str.bytes, 0, str.length << 1);
		if( p > 0 ) p >>= 1;
		return p;
	}

	public function lastIndexOf( str : String, ?startIndex : Int ) : Int {
		var last = 0;
		var start = this.length;
		if( startIndex != null )
			start = startIndex;
		start <<= 1;
		while( true ) {
			var p = bytes.find(last, (length << 1) - last, str.bytes, 0, str.length << 1);
			if( p < 0 || p > start )
				return (last >> 1) - 1;
			last = p + 2;
		}
		return -1;
	}

	public function split( delimiter : String ) : Array<String> {
		var out = [];
		if( length == 0 ) {
			out.push("");
			return out;
		}
		if( delimiter.length == 0 ) {
			for( i in 0...length )
				out.push(substr(i,1));
			return out;
		}
		var pos = 0;
		var dlen = delimiter.length;
		while( true ) {
			var p = bytes.find(pos << 1, (length - pos) << 1, delimiter.bytes, 0, dlen << 1);
			if( p < 0 ) {
				out.push(substr(pos, length-pos));
				break;
			}
			p >>= 1;
			out.push(substr(pos, p - pos));
			pos = p + dlen;
		}
		return out;
	}

	public function substr( pos : Int, ?len : Int ) : String @:privateAccess {
		var sl = length;
		var len : Int = if( len == null ) sl else len;
		if( len == 0 ) return "";
		if( pos != 0 && len < 0 )
			return "";
		if( pos < 0 ) {
			pos = sl + pos;
			if( pos < 0 ) pos = 0;
		} else if( len < 0 ) {
			len = sl + len - pos;
			if( len < 0 ) return "";
		}
		if( ((pos + len) : UInt) > (sl:UInt) )
			len = sl - pos;
		if( pos < 0 || len <= 0 ) return "";

		var b = new hl.Bytes((len + 1) << 1);
		b.blit(0, bytes, pos<<1, len << 1);
		b.setUI16(len<<1,0);
		return __alloc__(b, len);
	}

	public function substring( startIndex : Int, ?endIndex : Int ) : String {
		var end : Int;
		if( endIndex == null )
			end = length;
		else {
			end = endIndex;
			if( end < 0 )
				end = 0;
			else if ( end > length )
				end = length;
		}
		if( startIndex < 0 )
			startIndex = 0;
		else if ( startIndex > length )
			startIndex = length;
		if( startIndex > end ) {
			var tmp = startIndex;
			startIndex = end;
			end = tmp;
		}
		return substr( startIndex, end - startIndex );
	}

	public function toString() : String {
		return this;
	}

	public static function fromCharCode( code : Int ) : String {
		if( code >= 0 && code < 0x10000 ) {
			if( code >= 0xD800 && code <= 0xDFFF ) throw "Invalid unicode char " + code;
			var b = new hl.Bytes(4);
			b.setUI16(0, code);
			b.setUI16(2, 0);
			return __alloc__(b, 1);
		} else if( code < 0x110000 ) {
			var b = new hl.Bytes(6);
			code -= 0x10000;
			b.setUI16(0, (code >> 10) + 0xD800);
			b.setUI16(2, (code & 1023) + 0xDC00);
			b.setUI16(4, 0);
			return __alloc__(b, 2); // UTF16 encoding but UCS2 API (same as JS)
		} else
			throw "Invalid unicode char " + code;
	}

	function toUtf8() : hl.Bytes {
		return bytes.utf16ToUtf8(0, null);
	}

	@:keep function __string() : hl.Bytes {
		return bytes;
	}

	@:keep function __compare( s : String ) : Int {
		var v = bytes.compare(0, s.bytes, 0, (length < s.length ? length : s.length) << 1);
		return v == 0 ? length - s.length : v;
	}

	@:keep static inline function __alloc__( b : hl.Bytes, length : Int ) : String {
		var s : String = untyped $new(String);
		s.bytes = b;
		s.length = length;
		return s;
	}

	@:keep static function call_toString( v : Dynamic ) : hl.Bytes {
		var s : String = v.toString();
		return s.bytes;
	}

	inline static function fromUCS2( b : hl.Bytes ) : String {
		var s : String = untyped $new(String);
		s.bytes = b;
		s.length = @:privateAccess b.ucs2Length(0);
		return s;
	}

	@:keep static function fromUTF8( b : hl.Bytes ) : String {
		var outLen = 0;
		var b2 = @:privateAccess b.utf8ToUtf16(0, outLen);
		return __alloc__(b2, outLen>>1);
	}

	@:keep static function __add__( a : String, b : String ) : String {
		if( a == null ) a = "null";
		if( b == null ) b = "null";
		var asize = a.length << 1, bsize = b.length << 1, tot = asize + bsize;
		var bytes = new hl.Bytes(tot+2);
		bytes.blit(0, a.bytes, 0, asize);
		bytes.blit(asize,b.bytes,0,bsize);
		bytes.setUI16(tot, 0);
		return __alloc__(bytes, tot>>1);
	}
}
