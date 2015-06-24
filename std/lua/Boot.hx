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
			f = function(arg){
				return m(o,lua.Table.unpack(arg));
			};
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
				return (untyped __type__(o) == "number" &&  __lua__("_G.bit.bor(o,0) == o"));
			case Float:
				return untyped __type__(o) == "number";
			case Bool:
				return untyped __type__(o) == "boolean";
			case String:
				return untyped __type__(o) == "string";
			case Array:
				// TODO: Better array check
				return untyped __type__(o) == "table"
					&& o.__enum__ == null
					&& o.length != null;
			case Dynamic:
				return true;
			default:
				if( o != null ) {
					// Check if o is an instance of a Haxe class or a native Lua object
					if (untyped __type__(cl) == "table" ) {
						// TODO: Fixme
						return true;
					}
				} else {
					return false;
				}

				// do not use isClass/isEnum here
				untyped __feature__("Class.*",if( cl == Class && o.__name__ != null ) return true);
				untyped __feature__("Enum.*",if( cl == Enum && o.__ename__ != null ) return true);
				return o.__enum__ == cl;
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

	public static function urlEncode(str:String){
		if (str != null) {
			str = lua.StringTools.gsub(str, "\n", "\r\n");
			str = lua.StringTools.gsub(str, "([^%w %-%_%.%~])", function (c) {
				return lua.StringTools.format("%%%02X", lua.StringTools.byte(c) + '');
			});
			str = lua.StringTools.gsub(str, " ", "+");
		}
		return str;
	}

	static function printEnum(e:Array<Dynamic>){
		var params = e.slice(2).join(',');
		var first = e[0];
		return '$first($params)';
	}

	static function printClass(c:Table<String,Dynamic>, s : String) : String {
		return '{${printClassRec(c,'',s)}}';

	}

	static function printClassRec(c:Table<String,Dynamic>, result='', s : String) : String {
		c.pairsEach(function(k,v){
			if (result != "")
				result += ", ";
			result += '$k: ${__string_rec(v, s + 'o')}';
		});
		return result;
	}

	@:ifFeature("may_print_enum")
	static function __string_rec(o : Dynamic, s = '') {
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
				var isArray = untyped Lua.getmetatable(o).__index == Array.prototype;
				if (s.length > 5) isArray ? "[...]" : "{...}";
				else if (Reflect.hasField(o,"toString")) o.toString();
				else if (Reflect.hasField(o,"__tostring")) Lua.tostring(o);
				else if (Reflect.hasField(o,"__enum__")) printEnum(o);
				else if (Reflect.hasField(o,"__class__")) printClass(o,s);
				else if (Lua.next(o) == null) "{}";
				else {
					throw "Unknown Lua type";
					null;
				}
			};
			default : {
				throw "Unknown Lua type";
				null;
			}
		}

	}
}
