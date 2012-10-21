/*
 * Copyright (c) 2005, The haXe Project Contributors
 * All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   - Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   - Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE HAXE PROJECT CONTRIBUTORS "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE HAXE PROJECT CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 */
package haxe;

@:coreApi
class Utf8 {

	var __b : Void;

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
		var buf = utf8_buf_alloc( sl );
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