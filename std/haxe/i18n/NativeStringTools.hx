package haxe.i18n;

import haxe.i18n.Utf8.Utf8Reader;
import haxe.io.BytesData;

class NativeStringTools {

	public static function toUtf16 (s:String):ByteAccess {
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
		// fallback utf8 to utf16
		return Encoding.convertUtf8toUtf16(new Utf8Reader(toUtf8(s)), StrictConversion);
		#end
	}

	public static function toUtf32 (s:String):ByteAccess {
		return Encoding.convertUtf8toUtf32(new Utf8Reader(toUtf8(s)), StrictConversion);
	}

	public static function toUcs2 (s:String):ByteAccess {
		return Encoding.convertUtf8toUcs2(new Utf8Reader(toUtf8(s)), StrictConversion, false);
	}

	public static function toUtf8 (s:String):ByteAccess {
		#if neko
		return ByteAccess.ofData(untyped __dollar__ssub(s.__s,0,s.length));
		#elseif flash
		var b = new flash.utils.ByteArray();
		b.writeUTFBytes(s);
		return ByteAccess.ofData(b.length,b);
		#elseif php
		var x = BytesData.ofString(s);
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
		#else
		var a = new Array();
		// utf16-decode and utf8-encode
		var i = 0;
		while( i < s.length ) {
			var c : Int = StringTools.fastCodeAt(s,i++);
			// surrogate pair
			if( 0xD800 <= c && c <= 0xDBFF )
			       c = (c - 0xD7C0 << 10) | (StringTools.fastCodeAt(s,i++) & 0x3FF);
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
		#if js
		return ByteAccess.ofData(new js.html.Uint8Array(a).buffer);
		#else
		return ByteAccess.ofData(a);
		#end
		#end
	}
}