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
package haxe;

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

	public static function encode( s : String ) : String {
		s = untyped s.__s;
		var sl = untyped __dollar__ssize(s);
		var buf:Dynamic = utf8_buf_alloc( sl );
		var i = 0;
		while( i < sl ) {
			utf8_buf_add(buf,untyped __dollar__sget(s,i));
			i += 1;
		}
		return new String( utf8_buf_content(buf) );
	}

	public static function decode( s : String ) : String {
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

	public static function validate( s : String ) : Bool {
		return utf8_validate(untyped s.__s);
	}

	public static function length( s : String ) : Int {
		return utf8_length(untyped s.__s);
	}

	public static function compare( a : String, b : String ) : Int {
		return utf8_compare(untyped a.__s,untyped b.__s);
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