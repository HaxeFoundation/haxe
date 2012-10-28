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
@:noDoc
class HxOverrides {

	static function dateStr( date :Date ) : String {
		var m = date.getMonth() + 1;
		var d = date.getDate();
		var h = date.getHours();
		var mi = date.getMinutes();
		var s = date.getSeconds();
		return date.getFullYear()
			+"-"+(if( m < 10 ) "0"+m else ""+m)
			+"-"+(if( d < 10 ) "0"+d else ""+d)
			+" "+(if( h < 10 ) "0"+h else ""+h)
			+":"+(if( mi < 10 ) "0"+mi else ""+mi)
			+":"+(if( s < 10 ) "0"+s else ""+s);
	}

	static function strDate( s : String ) : Date {
		switch( s.length ) {
		case 8: // hh:mm:ss
			var k = s.split(":");
			var d : Date = untyped __new__(Date);
			untyped d["setTime"](0);
			untyped d["setUTCHours"](k[0]);
			untyped d["setUTCMinutes"](k[1]);
			untyped d["setUTCSeconds"](k[2]);
			return d;
		case 10: // YYYY-MM-DD
			var k = s.split("-");
			return new Date(cast k[0],cast untyped k[1] - 1,cast k[2],0,0,0);
		case 19: // YYYY-MM-DD hh:mm:ss
			var k = s.split(" ");
			var y = k[0].split("-");
			var t = k[1].split(":");
			return new Date(cast y[0],cast untyped y[1] - 1,cast y[2],cast t[0],cast t[1],cast t[2]);
		default:
			throw "Invalid date format : " + s;
		}
	}

	static function cca( s : String, index : Int ) : Null<Int> {
		#if mt
		var x = (cast s).cca(index);
		#else
		var x = (cast s).charCodeAt(index);
		#end
		if( x != x ) // fast isNaN
			return untyped undefined; // isNaN will still return true
		return x;
	}

	static function substr( s : String, pos : Int, ?len : Int ) : String {
		if( pos != null && pos != 0 && len != null && len < 0 ) return "";
		if( len == null ) len = s.length;
		if( pos < 0 ){
			pos = s.length + pos;
			if( pos < 0 ) pos = 0;
		}else if( len < 0 ){
			len = s.length + len - pos;
		}

		return (untyped s).substr(pos, len);
	}

	static function remove<T>( a : Array<T>, obj : T ) {
		var i = 0;
		var l = a.length;
		while( i < l ) {
			if( a[i] == obj ) {
				a.splice(i,1);
				return true;
			}
			i++;
		}
		return false;
	}

	static function iter<T>( a : Array<T> ) : Iterator<T> untyped {
		return {
			cur : 0,
			arr : a,
			hasNext : function() {
				return __this__.cur < __this__.arr.length;
			},
			next : function() {
				return __this__.arr[__this__.cur++];
			}
		};
	}

	static function __init__() untyped {
		__feature__('HxOverrides.remove',
			if( Array.prototype.indexOf ) __js__('HxOverrides').remove = function(a,o) {
				var i = a.indexOf(o);
				if( i == -1 ) return false;
				a.splice(i,1);
				return true;
			}
		);
		#if mt
		if( String.prototype.cca == null ) String.prototype.cca = String.prototype.charCodeAt;
		#end
	}

}
