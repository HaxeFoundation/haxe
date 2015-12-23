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

	static inline public function isClass(o:Dynamic) : Bool {
		if (Lua.type(o) != "table") return false;
		else return untyped __define_feature__("lua.Boot.isClass", o.__name__);
	}

	static inline public function isEnum(e:Dynamic) : Bool {
		if (Lua.type(e) != "table") return false;
		else return untyped __define_feature__("lua.Boot.isEnum", e.__ename__);
	}

	static inline public function getClass(o:Dynamic) : Dynamic {
		if (Std.is(o, Array)) return Array;
		else {
			var cl = untyped __define_feature__("lua.Boot.getClass", o.__class__);
			if (cl != null) return cl;
			else return null;
		}
	}

	@:ifFeature("typed_catch")
	private static function __instanceof(o : Dynamic, cl : Dynamic) {
		if( cl == null ) return false;

		switch( cl ) {
			case Int:
				// TODO: matching js behavior here, but js behavior clamps.  Is that correct?
				return (Lua.type(o) == "number" &&  clamp(o) == o);
			case Float:
				return Lua.type(o) == "number";
			case Bool:
				return Lua.type(o) == "boolean";
			case String:
				return Lua.type(o) == "string";
			case Array:
				return Lua.type(o) == "table"
					&& untyped o.__enum__ == null
					&& lua.Lua.getmetatable(o) != null
					&& lua.Lua.getmetatable(o).__index == untyped Array.prototype;
			case Table:
				return Lua.type(o) == "table";
			case Dynamic:
				return true;
			default: {
				if ( o!= null &&  Lua.type(o)  == "table" && Lua.type(cl) == "table"){
					// first check if o is instance of cl
					if (inheritsFrom(o, cl)) return true;

					// do not use isClass/isEnum here, perform raw checks
					untyped __feature__("Class.*",if( cl == Class && o.__name__ != null ) return true);
					untyped __feature__("Enum.*",if( cl == Enum && o.__ename__ != null ) return true);
					// last chance, is it an enum instance?
					return o.__enum__ == cl;
				} else {
					return false;
				}
			}
		}
	}
	static function inheritsFrom(o:Dynamic, cl:Class<Dynamic>) : Bool {
		while (Lua.getmetatable(o) != null && Lua.getmetatable(o).__index != null){
			if (Lua.getmetatable(o).__index == untyped cl.prototype) return true;
			o = Lua.getmetatable(o).__index;
		}
		return false;
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

	static function printEnum(o:Table<Int,Dynamic>, s : String){
		if (!Std.is(o, Array)){
			return o[0];
		} else {
			var o2 : Array<Dynamic> = cast o;
			var str = o[0] + "(";
			s += "\t";
			for (i in 2...o2.length){
				if( i != 2 )
					str += "," + __string_rec(o[i],s);
				else
					str += __string_rec(o[i],s);
			}
			return str + ")";
		}
	}

	static function printClass(c:Table<String,Dynamic>, s : String) : String {
		return '{${printClassRec(c,'',s)}}';

	}

	static function printClassRec(c:Table<String,Dynamic>, result='', s : String) : String {
		c.pairsEach(function(k,v){
			if (result != "")
				result += ", ";
			result += '$k: ${__string_rec(v, s + "\t")}';
		});
		return result;
	}

	@:ifFeature("has_enum")
	static function __string_rec(o : Dynamic, s:String = "") {
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
			    if (Reflect.hasField(o,"__enum__")) printEnum(o,s);
				else if (o.toString != null && !__instanceof(o,Array)) o.toString();
				else if (__instanceof(o, Array)) {
					if (s.length > 5) "[...]"
					else '[${[for (i in cast(o,Array<Dynamic>)) __string_rec(i,s+1)].join(",")}]';
				} else if (s.length > 5){
					"{...}";
				}
				else if (Reflect.hasField(o,"__tostring")) Lua.tostring(o);
				else if (Reflect.hasField(o,"__class__")) printClass(o,s+"\t");
				else if (Lua.next(o) == null) "{}";
				else {
					var fields = Reflect.fields(o);
					var buffer = new StringBuf();
					for (f in fields){
						buffer.add('${s}${Std.string(f)} : ${untyped Std.string(o[f])}');
					}
					buffer.toString();
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

	public static function clamp(x:Int){
		return (x & 2147483647) - (x & cast 2147483648);
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
