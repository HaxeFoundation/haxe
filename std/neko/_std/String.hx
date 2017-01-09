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
@:coreApi @:final class String {

	static var __is_String;
	private static var __split : Dynamic = neko.Lib.load("std","string_split",2);

	static function __init__() : Void {
		__is_String = true;
	}

	public var length(default,null) : Int;

	public function new(string:String) : Void {
		untyped {
			if( __dollar__typeof(string) != __dollar__tstring )
				string = __dollar__string(string);
			this.__s = string;
			this.length = __dollar__ssize(string);
		}
	}

	public function charAt(index:Int) : String {
		untyped {
			try {
				var s = __dollar__smake(1);
				__dollar__sset(s,0,__dollar__sget(this.__s,index));
				return new String(s);
			} catch( e : Dynamic ) {
				return "";
			}
		}
	}

	public function charCodeAt(index : Int) : Null<Int> {
		untyped {
			return __dollar__sget(this.__s,index);
		}
	}

	public function indexOf( str : String, ?startIndex : Int ) : Int {
		untyped {
			var p = try __dollar__sfind(this.__s,if( startIndex == null ) 0 else startIndex,str.__s) catch( e : Dynamic ) null;
			if( p == null )
				return -1;
			return p;
		}
	}

	public function lastIndexOf( str : String, ?startIndex : Int ) : Int {
		untyped {
			var last = -1;
			if( startIndex == null )
				startIndex = __dollar__ssize(this.__s);
			while( true ) {
				var p = try __dollar__sfind(this.__s,last+1,str.__s) catch( e : Dynamic ) null;
				if( p == null || p > startIndex )
					return last;
				last = p;
			}
		}
	}

	public function split( delimiter : String ) : Array<String> {
		untyped {
			var l = __split(this.__s,delimiter.__s);
			var a = new Array<String>();
			if( l == null ) {
				a.push("");
				return a;
			}
			do {
				a.push(new String(l[0]));
				l = l[1];
			} while( l != null );
			return a;
		}
	}

	public function substr( pos : Int, ?len : Int ) : String {
		if( len == 0 ) return "";
		var sl = length;

		if( len == null ) len = sl;

		if( pos == null ) pos = 0;
		if( pos != 0 && len < 0 ){
			return "";
		}

		if( pos < 0 ){
			pos = sl + pos;
			if( pos < 0 ) pos = 0;
		}else if( len < 0 ){
			len = sl + len - pos;
		}

		if( pos + len > sl ){
			len = sl - pos;
		}

		if( pos < 0 || len <= 0 ) return "";
		return new String(untyped __dollar__ssub(this.__s,pos,len));
	}

	public function substring( startIndex : Int, ?endIndex : Int ) : String {
		if ( endIndex == null) {
			endIndex = length;
		} else if ( endIndex < 0 ) {
			endIndex = 0;
		} else if ( endIndex > length ) {
			endIndex = length;
		}

		if ( startIndex < 0 ) {
			startIndex = 0;
		} else if ( startIndex > length ) {
			startIndex = length;
		}

		if ( startIndex > endIndex ) {
			var tmp = startIndex;
			startIndex = endIndex;
			endIndex = tmp;
		}

		return substr( startIndex, endIndex - startIndex );
	}

	public function toLowerCase() : String {
		untyped {
			var s = this.__s;
			var l = this.length;
			var s2 = __dollar__scopy(s);
			var i = 0;
			while( i < l ) {
				var c = __dollar__sget(s,i);
				if( c >= 65 && c <= 90 )
					__dollar__sset(s2,i,c-65+97);
				i++;
			}
			return new String(s2);
		}
	}

	public function toUpperCase() : String {
		untyped {
			var s = this.__s;
			var l = this.length;
			var s2 = __dollar__scopy(s);
			var i = 0;
			while( i < l ) {
				var c = __dollar__sget(s,i);
				if( c >= 97 && c <= 122 )
					__dollar__sset(s2,i,c-97+65);
				i++;
			}
			return new String(s2);
		}
	}

	public function toString() : String {
		return this;
	}

	/* NEKO INTERNALS */

	private function __compare(o:String) : Int {
		return untyped __dollar__compare(this.__s,o.__s);
	}

	private function __add(s:Dynamic) : String {
		return new String(untyped this.__s+__dollar__string(s));
	}

	private function __radd(s:Dynamic) : String {
		return new String(untyped __dollar__string(s)+this.__s);
	}

	public static function fromCharCode( code : Int ) : String untyped {
		var s = __dollar__smake(1);
		__dollar__sset(s,0,code);
		return new String(s);
	}

}
