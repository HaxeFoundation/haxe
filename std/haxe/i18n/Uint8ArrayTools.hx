package haxe.i18n;

import haxe.io.BytesData;
import haxe.io.Error;

#if !nodejs
import js.html.compat.Uint8Array;
#end
import js.html.Uint8Array;

class Uint8ArrayTools {

	#if (js)
	public static inline function get( b:Uint8Array, pos : Int ) : Int {
		return b[pos];
	}

	public static inline function set( b:Uint8Array, pos : Int, v : Int ) : Void {
		b[pos] = v & 0xFF;
	}
	public static inline function getString( b:Uint8Array, pos : Int, len : Int ) : String {
		if( pos < 0 || len < 0 || pos + len > getLength(b) ) throw Error.OutsideBounds;
		var s = "";

		var fcc = String.fromCharCode;
		var i = pos;
		var max = pos+len;
		// utf8-decode and utf16-encode
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
				var u = ((c & 0x0F) << 18) | ((c2 & 0x7F) << 12) | ((c3 & 0x7F) << 6) | (b[i++] & 0x7F);
				// surrogate pair
				s += fcc( (u >> 10) + 0xD7C0 );
				s += fcc( (u & 0x3FF) | 0xDC00 );
			}
		}
		return s;
	}
	public static inline function alloc (length:Int) {
		var data = new BytesData(length);
		return Uint8ArrayTools.wrapData(data);
	}
	public static inline function sub(b:Uint8Array, pos:Int, len:Int):Uint8Array {
		if( pos < 0 || len < 0 || pos + len > getLength(b) ) throw Error.OutsideBounds;
		return wrapData(b.buffer.slice(pos+b.byteOffset,pos+b.byteOffset+len));
	}


	public static inline function fastGet (b:Uint8Array, pos:Int):Int {
		return (b:Dynamic)[pos];
	}

	public static inline function getData(b:Uint8Array):BytesData {
		return (b:Dynamic).bufferValue;
	}

	public static inline function getLength (b:Uint8Array):Int {
		return getData(b).byteLength;
	}
	public static inline function wrapData (data:BytesData):Uint8Array {
		var a = new js.html.Uint8Array(data);
		(a:Dynamic).bufferValue = data; // some impl does not return the same instance in .buffer
		(data:Dynamic).bytes = a;
		return a;
	}
	#end

}