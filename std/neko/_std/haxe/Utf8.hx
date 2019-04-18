/*
 * Copyright (C)2005-2019 Haxe Foundation
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
package haxe;

import haxe.io.Bytes;

@:coreApi
class Utf8 {

	var __b : Dynamic;

	public function new( ?size : Int ) : Void {
		__b = utf8_buf_alloc(if( size == null ) 1 else size);
	}

	public function addChar( c : Int ) : Void {
		utf8_buf_add(__b,c);
	}

	public function toString() : String {
		return new String(utf8_buf_content(__b));
	}

	public static function encode( s : String ) : Bytes {
		s = untyped s.__s;
		var sl = untyped __dollar__ssize(s);
		var buf:Dynamic = utf8_buf_alloc( sl );
		var i = 0;
		while( i < sl ) {
			utf8_buf_add(buf,untyped __dollar__sget(s,i));
			i += 1;
		}
		return Bytes.ofString(new String( utf8_buf_content(buf) ), RawNative);
	}

	public static function decode( b : Bytes ) : String {
		var s = b.getString(0, b.length, RawNative);
		s = untyped s.__s;
		var sl = untyped __dollar__ssize(s);
		var ret = untyped __dollar__smake(sl);
		var i = 0;
		utf8_iter(s,function(c) {
			if( c == 8364 ) // euro symbol
				c = 164;
			else if( c == 0xFEFF ) // BOM
				return;
			else if( c > 255 )
				throw "Utf8::decode invalid character ("+c+")";
			untyped __dollar__sset(ret,i,c);
			i += 1;
		});
		return new String( untyped __dollar__ssub(ret,0,i) );
	}

	public static function iter( s : String, chars : Int -> Void ) : Void {
		utf8_iter(untyped s.__s,chars);
	}

	public static function charCodeAt( s : String, index : Int ) : Int {
		return utf8_get(untyped s.__s,index);
	}

	public static function validate( b : Bytes ) : Bool {
		var data = b.getData();
		var pos = 0;
		var max = b.length;
		while( pos < max ) {
			var c:Int = Bytes.fastGet(data, pos++);
			if( c < 0x80 ) {
			} else if( c < 0xC0 ) { // invalid continuation byte
				return false;
			} else if( c < 0xC2 ) { // overlong sequence
				return false;
			} else if( c < 0xE0 ) {
				if( pos + 1 > max ) return false;
				var c2:Int = Bytes.fastGet(data, pos++);
				if( c2 < 0x80 || c2 > 0xBF ) return false;
			} else if( c < 0xF0 ) {
				if( pos + 2 > max ) return false;
				var c2:Int = Bytes.fastGet(data, pos++);
				if( c == 0xE0 ) {
					if( c2 < 0xA0 || c2 > 0xBF ) return false;
				} else {
					if( c2 < 0x80 || c2 > 0xBF ) return false;
				}
				var c3:Int = Bytes.fastGet(data, pos++);
				if( c3 < 0x80 || c3 > 0xBF ) return false;
				c = (c << 16) | (c2 << 8) | c3;
				if(0xEDA080 <= c && c <= 0xEDBFBF) return false; //surrogate pairs
			} else if( c > 0xF4 ) {
				return false;
			} else {
				if( pos + 3 > max ) return false;
				var c2:Int = Bytes.fastGet(data, pos++);
				if( c == 0xF0 ) {
					if( c2 < 0x90 || c2 > 0xBF ) return false;
				} else if( c == 0xF4 ) {
					if( c2 < 0x80 || c2 > 0x8F ) return false;
				} else {
					if( c2 < 0x80 || c2 > 0xBF ) return false;
				}
				var c3:Int = Bytes.fastGet(data, pos++);
				if( c3 < 0x80 || c3 > 0xBF ) return false;
				var c4:Int = Bytes.fastGet(data, pos++);
				if( c4 < 0x80 || c4 > 0xBF ) return false;
			}
		}
		return true;
	}

	public static function length( s : String ) : Int {
		return utf8_length(untyped s.__s);
	}

	public static function sub( s : String, pos : Int, len : Int ) : String {
		return new String(utf8_sub(untyped s.__s,pos,len));
	}

	static var utf8_buf_alloc = neko.Lib.load("std","utf8_buf_alloc",1);
	static var utf8_buf_add = neko.Lib.load("std","utf8_buf_add",2);
	static var utf8_buf_content = neko.Lib.load("std","utf8_buf_content",1);
	static var utf8_buf_length = neko.Lib.load("std","utf8_buf_length",1);
	static var utf8_iter = neko.Lib.load("std","utf8_iter",2);

	static var utf8_get = neko.Lib.load("std","utf8_get",2);
	static var utf8_validate = neko.Lib.load("std","utf8_validate",1);
	static var utf8_length = neko.Lib.load("std","utf8_length",1);
	static var utf8_compare = neko.Lib.load("std","utf8_compare",2);
	static var utf8_sub = neko.Lib.load("std","utf8_sub",3);

}