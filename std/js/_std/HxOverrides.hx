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
		var x = (cast s).charCodeAt(index);
		if( x != x ) // fast isNaN
			return js.Lib.undefined; // isNaN will still return true
		return x;
	}

	static function substr( s : String, pos : Int, ?len : Int ) : String {
		if (len == null) {
			len = s.length;
		} else if (len < 0) {
			if (pos == 0)
				len = s.length + len;
			else
				return "";
		}

		#if (js_es < 5)
		if (pos < 0) {
			pos = s.length + pos;
			if (pos < 0)
				pos = 0;
		}
		#end

		return (untyped s).substr(pos, len);
	}

	static function indexOf<T>( a : Array<T>, obj : T, i : Int) {
		var len = a.length;
		if (i < 0) {
			i += len;
			if (i < 0) i = 0;
		}
		while (i < len)
		{
			if (untyped __js__("a[i] === obj"))
				return i;
			i++;
		}
		return -1;
	}

	static function lastIndexOf<T>( a : Array<T>, obj : T, i : Int) {
		var len = a.length;
		if (i >= len)
			i = len - 1;
		else if (i < 0)
			i += len;
		while (i >= 0)
		{
			if (untyped __js__("a[i] === obj"))
				return i;
			i--;
		}
		return -1;
	}

	static function remove<T>( a : Array<T>, obj : T ) {
		var i = a.indexOf(obj);
		if( i == -1 ) return false;
		a.splice(i,1);
		return true;
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
#if (js_es < 5)
		__feature__('HxOverrides.indexOf', if( Array.prototype.indexOf ) __js__("HxOverrides").indexOf = function(a,o,i) return Array.prototype.indexOf.call(a, o, i));
		__feature__('HxOverrides.lastIndexOf', if( Array.prototype.lastIndexOf ) __js__("HxOverrides").lastIndexOf = function(a,o,i) return Array.prototype.lastIndexOf.call(a, o, i));
#end
	}

}
