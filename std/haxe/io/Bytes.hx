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
package haxe.io;

class Bytes {

	public var length(default,null) : Int;
	var b : BytesData;

	function new(length,b) {
		this.length = length;
		this.b = b;
	}

	public inline function get( pos : Int ) : Int {
		#if neko
		return untyped __dollar__sget(b,pos);
		#elseif flash9
		return b[pos];
		#elseif php
		return untyped __call__("ord", b[pos]);
		#elseif cpp
		return untyped b[pos];
		#elseif java
		return untyped b[pos] & 0xFF;
		#else
		return b[pos];
		#end
	}

	public inline function set( pos : Int, v : Int ) : Void {
		#if neko
		untyped __dollar__sset(b,pos,v);
		#elseif flash9
		b[pos] = v;
		#elseif php
		b[pos] = untyped __call__("chr", v);
		#elseif cpp
		untyped b[pos] = v;
		#elseif java
		b[pos] = cast v;
		#elseif cs
		b[pos] = cast v;
		#else
		b[pos] = v & 0xFF;
		#end
	}

	public function blit( pos : Int, src : Bytes, srcpos : Int, len : Int ) : Void {
		#if !neko
		if( pos < 0 || srcpos < 0 || len < 0 || pos + len > length || srcpos + len > src.length ) throw Error.OutsideBounds;
		#end
		#if neko
		try untyped __dollar__sblit(b,pos,src.b,srcpos,len) catch( e : Dynamic ) throw Error.OutsideBounds;
		#elseif php
		// TODO: test me
		b = untyped __php__("substr($this->b, 0, $pos) . substr($src->b, $srcpos, $len) . substr($this->b, $pos+$len)"); //__call__("substr", b, 0, pos)+__call__("substr", src.b, srcpos, len)+__call__("substr", b, pos+len);
		#elseif flash9
		b.position = pos;
		if( len > 0 ) b.writeBytes(src.b,srcpos,len);
		#elseif java
		java.lang.System.arraycopy(src.b, srcpos, b, pos, len);
		#elseif cs
		cs.system.Array.Copy(src.b, srcpos, b, pos, len);
		#else
		var b1 = b;
		var b2 = src.b;
		if( b1 == b2 && pos > srcpos ) {
			var i = len;
			while( i > 0 ) {
				i--;
				b1[i + pos] = b2[i + srcpos];
			}
			return;
		}
		for( i in 0...len )
			b1[i+pos] = b2[i+srcpos];
		#end
	}

	public function sub( pos : Int, len : Int ) : Bytes {
		#if !neko
		if( pos < 0 || len < 0 || pos + len > length ) throw Error.OutsideBounds;
		#end
		#if neko
		return try new Bytes(len,untyped __dollar__ssub(b,pos,len)) catch( e : Dynamic ) throw Error.OutsideBounds;
		#elseif flash9
		b.position = pos;
		var b2 = new flash.utils.ByteArray();
		b.readBytes(b2,0,len);
		return new Bytes(len,b2);
		#elseif php
		// TODO: test me
		return new Bytes(len, untyped __call__("substr", b, pos, len));
		#elseif java
		var newarr = new java.NativeArray(len);
		java.lang.System.arraycopy(b, pos, newarr, 0, len);
		return new Bytes(len, newarr);
		#elseif cs
		var newarr = new cs.NativeArray(len);
		cs.system.Array.Copy(b, pos, newarr, 0, len);
		return new Bytes(len, newarr);
		#else
		return new Bytes(len,b.slice(pos,pos+len));
		#end
	}

	public function compare( other : Bytes ) : Int {
		#if neko
		return untyped __dollar__compare(b,other.b);
		#elseif flash9
		var len = (length < other.length) ? length : other.length;
		var b1 = b;
		var b2 = other.b;
		b1.position = 0;
		b2.position = 0;
		for( i in 0...len>>2 )
			if( b1.readUnsignedInt() != b2.readUnsignedInt() ) {
				b1.position -= 4;
				b2.position -= 4;
				return b1.readUnsignedInt() - b2.readUnsignedInt();
			}
		for( i in 0...len & 3 )
			if( b1.readUnsignedByte() != b2.readUnsignedByte() )
				return b1[b1.position-1] - b2[b2.position-1];
		return length - other.length;
		#elseif php
		return untyped __php__("$this->b < $other->b ? -1 : ($this->b == $other->b ? 0 : 1)");
		//#elseif cs
		//TODO: memcmp if unsafe flag is on
		#else
		var b1 = b;
		var b2 = other.b;
		var len = (length < other.length) ? length : other.length;
		for( i in 0...len )
			if( b1[i] != b2[i] )
				#if cpp
				return untyped b1[i] - untyped b2[i];
				#else
				return untyped b1[i] - untyped b2[i];
				#end
		return length - other.length;
		#end
	}

	public function readString( pos : Int, len : Int ) : String {
		#if !neko
		if( pos < 0 || len < 0 || pos + len > length ) throw Error.OutsideBounds;
		#end
		#if neko
		return try new String(untyped __dollar__ssub(b,pos,len)) catch( e : Dynamic ) throw Error.OutsideBounds;
		#elseif flash9
		b.position = pos;
		return b.readUTFBytes(len);
		#elseif php
		// TODO: test me
		return untyped __call__("substr", b, pos, len);
//		return untyped __call__("call_user_func_array", "pack", __call__("array_merge", __call__("array", "C*"), __call__("array_slice", b.»a, pos, len)));
		#elseif cpp
		var result:String="";
		untyped __global__.__hxcpp_string_of_bytes(b,result,pos,len);
		return result;
		#elseif cs
		return cs.system.text.Encoding.UTF8.GetString(b, pos, len);
		#elseif java
		try
			return new String(b, pos, len, "UTF-8")
		catch (e:Dynamic) throw e;
		#else
		var s = "";
		var b = b;
		var fcc = String.fromCharCode;
		var i = pos;
		var max = pos+len;
		// utf8-encode
		while( i < max ) {
			var c = b[i++];
			if( c < 0x80 ) {
				if( c == 0 ) break;
				s += fcc(c);
			} else if( c < 0xE0 )
				s += fcc( ((c & 0x3F) << 6) | (b[i++] & 0x7F) );
			else if( c < 0xF0 ) {
				var c2 = b[i++];
				s += fcc( ((c & 0x1F) << 12) | ((c2 & 0x7F) << 6) | (b[i++] & 0x7F) );
			} else {
				var c2 = b[i++];
				var c3 = b[i++];
				s += fcc( ((c & 0x0F) << 18) | ((c2 & 0x7F) << 12) | ((c3 << 6) & 0x7F) | (b[i++] & 0x7F) );
			}
		}
		return s;
		#end
	}

	public function toString() : String {
		#if neko
		return new String(untyped __dollar__ssub(b,0,length));
		#elseif flash9
		b.position = 0;
		return b.readUTFBytes(length);
		#elseif php
		// TODO: test me
		return cast b;
//		return untyped __call__("call_user_func_array", "pack", __call__("array_merge", __call__("array", "C*"), b.»a));
		#elseif cs
		return cs.system.text.Encoding.UTF8.GetString(b, 0, length);
		#elseif java
		try
		{
			return new String(b, 0, length, "UTF-8");
		}
		catch (e:Dynamic) throw e;
		#else
		return readString(0,length);
		#end
	}

	public function toHex() : String {
		var s = new StringBuf();
		var chars = [];
		var str = "0123456789abcdef";
		for( i in 0...str.length )
			chars.push(str.charCodeAt(i));
		for( i in 0...length ) {
			var c = get(i);
			s.addChar(chars[c >> 4]);
			s.addChar(chars[c & 15]);
		}
		return s.toString();
	}

	public inline function getData() : BytesData {
		return b;
	}

	public static function alloc( length : Int ) : Bytes {
		#if neko
		return new Bytes(length,untyped __dollar__smake(length));
		#elseif flash9
		var b = new flash.utils.ByteArray();
		b.length = length;
		return new Bytes(length,b);
		#elseif php
		// TODO: test me
		return new Bytes(length, untyped __call__("str_repeat", __call__("chr", 0), length));
		/*
		if(length > 0)
			return new Bytes(length, untyped __call__("new _hx_array", __call__("array_fill", 0, length, 0)));
		else
			return new Bytes(0, untyped __call__("new _hx_array", __call__("array")));
		*/
		#elseif cpp
		var a = new BytesData();
		if (length>0) a[length-1] = untyped 0;
		return new Bytes(length, a);
		#elseif cs
		return new Bytes(length, new cs.NativeArray(length));
		#elseif java
		return new Bytes(length, new java.NativeArray(length));
		#else
		var a = new Array();
		for( i in 0...length )
			a.push(0);
		return new Bytes(length,a);
		#end
	}

	public static function ofString( s : String ) : Bytes {
		#if neko
		return new Bytes(s.length,untyped __dollar__ssub(s.__s,0,s.length));
		#elseif flash9
		var b = new flash.utils.ByteArray();
		b.writeUTFBytes(s);
		return new Bytes(b.length,b);
		#elseif php
		return new Bytes(untyped __call__("strlen", s), cast s);
//		return ofData(untyped __call__("new _hx_array", __call__("array_values", __call__("unpack", "C*",  s))));
		#elseif cpp
		var a = new BytesData();
		untyped __global__.__hxcpp_bytes_of_string(a,s);
		return new Bytes(a.length, a);
		#elseif cs
		var b = cs.system.text.Encoding.UTF8.GetBytes(s);
		return new Bytes(b.Length, b);
		#elseif java
		try
		{
			var b:BytesData = untyped s.getBytes("UTF-8");
			return new Bytes(b.length, b);
		}
		catch (e:Dynamic) throw e;
		#else
		var a = new Array();
		// utf8-decode
		for( i in 0...s.length ) {
			var c : Int = StringTools.fastCodeAt(s,i);
			if( c <= 0x7F )
				a.push(c);
			else if( c <= 0x7FF ) {
				a.push( 0xC0 | (c >> 6) );
				a.push( 0x80 | (c & 63) );
			} else if( c <= 0xFFFF ) {
				a.push( 0xE0 | (c >> 12) );
				a.push( 0x80 | ((c >> 6) & 63) );
				a.push( 0x80 | (c & 63) );
			} else {
				a.push( 0xF0 | (c >> 18) );
				a.push( 0x80 | ((c >> 12) & 63) );
				a.push( 0x80 | ((c >> 6) & 63) );
				a.push( 0x80 | (c & 63) );
			}
		}
		return new Bytes(a.length,a);
		#end
	}

	public static function ofData( b : BytesData ) {
		#if flash9
		return new Bytes(b.length,b);
		#elseif neko
		return new Bytes(untyped __dollar__ssize(b),b);
		#elseif php
		return new Bytes(untyped __call__("strlen", b), b);
		#elseif cs
		return new Bytes(b.Length,b);
		#else
		return new Bytes(b.length,b);
		#end
	}

	/**
		Read the most efficiently possible the n-th byte of the data.
		Behavior when reading outside of the available data is unspecified.
	**/
	public inline static function fastGet( b : BytesData, pos : Int ) : Int {
		#if neko
		return untyped __dollar__sget(b,pos);
		#elseif flash9
		return b[pos];
		#elseif php
		return untyped __call__("ord", b[pos]);
		#elseif cpp
		return untyped b[pos];
		#elseif java
		return untyped b[pos] & 0xFF;
		#else
		return b[pos];
		#end
	}

}
