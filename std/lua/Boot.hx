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
package lua;

import lua.Table;
import haxe.Constraints.Function;
using lua.PairTools;

class Boot {

	// Used temporarily for bind()
	static var _;
	static var _fid = 0;

	static function __unhtml(s : String)
		return s.split("&").join("&amp;").split("<").join("&lt;").split(">").join("&gt;");

	public static function patternQuote(str:String)
		return lua.StringTools.gsub(str, "[%(%)%.%%%+%-%*%?%[%]%^%$]", function(c:String){ return "%" + c; });

	public inline static function tableToArray<T>(t:Table<Int,T>, ?length:Int) : Array<T> {
		if (length == null) length = Table.maxn(t);
		return cast defArray(t,length);
	}

	public static function luaIteratorToArray<T>(itr:Void->T) : Array<T> {
		var i: T = null;
		var ret : Array<T> = [];
		while({i = itr(); i != null;}){
			ret.push(i);
		}
		return ret;
	}

	@:keep
	public static function bind(o:Dynamic, m: Function) : Function{
		if (m == null) return null;
		// if (m.__id.__ == nil) m.__id__ = _fid + 1;
		var f: Function = null;
		if ( o.hx__closures__ == null ) o.hx__closures__ = {};
		else untyped f = o.hx__closures__[m];
		if (f == null){
			f = untyped __lua__("function(...) return m(o, ...) end");
			untyped o.hx__closures__[m] = f;
		}
		return f;
	}

	static inline function isClass(o:Dynamic) : Bool {
		return untyped __define_feature__("lua.Boot.isClass", o.__name__);
	}

	static inline function isEnum(e:Dynamic) : Bool {
		return untyped __define_feature__("lua.Boot.isEnum", e.__ename__);
	}

	static inline function getClass(o:Dynamic) : Dynamic {
		if (Std.is(o, Array)) return Array;
		else {
			var cl = untyped __define_feature__("lua.Boot.getClass", o.__class__);
			if (cl != null) return cl;
			else return null;
		}
	}

	@:ifFeature("typed_catch")
	private static function __instanceof(o : Dynamic,cl : Dynamic) {
		if( cl == null ) return false;

		switch( cl ) {
			case Int:
				return (untyped __type__(o) == "number" &&  lua.Math.floor(o) == o);
			case Float:
				return untyped __type__(o) == "number";
			case Bool:
				return untyped __type__(o) == "boolean";
			case String:
				return untyped __type__(o) == "string";
			case Array:
				return untyped __type__(o) == "table"
					&& o.mt != null
					&& o.mt__index == untyped Array.prototype;
			case Table:
				return untyped __type__(o) == "table";
			case Dynamic:
				return true;
			default: {
				if (   untyped __type__(o)  == "table"
					&& untyped __type__(cl) == "table"){
					while (Lua.getmetatable(o).__index != null){
						if (Lua.getmetatable(o).__index == cl.prototype) return true;
						o = Lua.getmetatable(o).__index;
					}
					return false;
				}

				return false;
			}
		}
	}

	@:ifFeature("typed_cast")
	private static function __cast(o : Dynamic, t : Dynamic) {
		if (__instanceof(o, t)) return o;
		else throw "Cannot cast " +Std.string(o) + " to " +Std.string(t);
	}

	@:keep
	public static function arrayNewIndex(tab:Dynamic, key:Dynamic, value:Dynamic){
		untyped rawset(tab, key, value);
		if (Std.is(key,Int) && key+1 > tab.length){
			tab.length = key + 1;
		}
	}

	@:keep
	public static function defArray(tabobj: Dynamic, length : Int) : Array<Dynamic>  untyped {
		tabobj.length = length;
		setmetatable(tabobj, {
			__index : __lua__("Array.prototype"),
			__newindex : lua.Boot.arrayNewIndex
		});
		return tabobj;
	}

	public static function urlDecode(str:String){
		str = lua.StringTools.gsub (str, "+", " ");
		str = lua.StringTools.gsub (str, "%%(%x%x)",
				function(h) {return lua.StringTools.char(lua.Lua.tonumber(h,16));});
		str = lua.StringTools.gsub (str, "\r\n", "\n");
		return str;
	}

	public static function urlEncode(str:String){
		str = lua.StringTools.gsub(str, "\n", "\r\n");
		str = lua.StringTools.gsub(str, "([^%w %-%_%.%~])", function (c) {
			return lua.StringTools.format("%%%02X", lua.StringTools.byte(c) + '');
		});
		str = lua.StringTools.gsub(str, " ", "+");
		return str;
	}

	static function printEnum(e:Array<Dynamic>){
		var params = e.slice(2).join(',');
		var first = e[0];
		return '$first($params)';
	}

	static function printClass(c:Table<String,Dynamic>, s : Int) : String {
		return '{${printClassRec(c,'',s)}}';

	}

	static function printClassRec(c:Table<String,Dynamic>, result='', s : Int) : String {
		c.pairsEach(function(k,v){
			if (result != "")
				result += ", ";
			result += '$k: ${__string_rec(v, s + 1)}';
		});
		return result;
	}

	@:ifFeature("may_print_enum")
	static function __string_rec(o : Dynamic, s = 0) {
		return switch(untyped __type__(o)){
			case "nil": "null";
			case "number" : {
				if (o == std.Math.POSITIVE_INFINITY) "Infinity";
				else if (o == std.Math.NEGATIVE_INFINITY) "-Infinity";
				else if (o != o) "NaN";
				else untyped tostring(o);
			}
			case "boolean" : untyped tostring(o);
			case "string"  : o;
			case "userdata": "<userdata>";
			case "function": "<function>";
			case "thread"  : "<thread>";
			case "table": {
				var mt : Dynamic = untyped Lua.getmetatable(o);
			    if (Reflect.hasField(o,"__enum__")) printEnum(o);
				else if (Lua.next(o) == null) "{}";
				else if (Reflect.hasField(o,"toString") && !__instanceof(o,Array)) o.toString();
				else if (__instanceof(o, Array)) {
					if (s > 5) "[...]"
					else '[${[for (i in cast(o,Array<Dynamic>)) __string_rec(i,s+1)].join(",")}]';
				} else if (s > 5){
					"{...}";
				}
				else if (Reflect.hasField(o,"__tostring")) Lua.tostring(o);
				else if (Reflect.hasField(o,"__class__")) printClass(o,s+1);
				else {
					cast(o, Table<Dynamic,Dynamic>).pairsFold(function(a,b,c){
						if (c != "{") c+= ", ";
						return c + __string_rec(a,s + 1) + ': ' + __string_rec(b, s + 1);
					},"{") + "}";
				}
			};
			default : {
				throw "Unknown Lua type";
				null;
			}
		}

	}

	public static function dateStr( date : std.Date ) : String {
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

	public static function strDate( s : String ) : std.Date {
		switch( s.length ) {
		case 8: // hh:mm:ss
			var k = s.split(":");
			var t = lua.Os.time({
				year  : 0,
				month : 1,
				day   : 1,
				hour  : Std.parseInt(k[0]),
				min   : Std.parseInt(k[1]),
				sec   : Std.parseInt(k[2])
			});
			return std.Date.fromTime(t);
		case 10: // YYYY-MM-DD
			var k = s.split("-");
			return new std.Date(Std.parseInt(k[0]), Std.parseInt(k[1]) - 1, Std.parseInt(k[2]),0,0,0);
		case 19: // YYYY-MM-DD hh:mm:ss
			var k = s.split(" ");
			var y = k[0].split("-");
			var t = k[1].split(":");
			return new std.Date(cast y[0],Std.parseInt(y[1]) - 1, Std.parseInt(y[2]),Std.parseInt(t[0]),Std.parseInt(t[1]),Std.parseInt(t[2]));
		default:
			throw "Invalid date format : " + s;
		}
	}
}
