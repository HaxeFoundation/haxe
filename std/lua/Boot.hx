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

class Boot {
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

	@:ifFeature("typed_catch") private static function __instanceof(o : Dynamic,cl : Dynamic) {
		if( cl == null ) return false;

		switch( cl ) {
			case Int:
				return (untyped __lua__("bitor(o,0) == o"));
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

	@:ifFeature("typed_cast") private static function __cast(o : Dynamic, t : Dynamic) {
		if (__instanceof(o, t)) return o;
		else throw "Cannot cast " +Std.string(o) + " to " +Std.string(t);
	}

	@:keep
	public static function arrayNewIndex(tab:Dynamic, key:Int, value:Dynamic){
		untyped rawset(tab, key, value);
		if (key+1 > tab.length){
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

	@:ifFeature("may_print_enum")
	static function __string_rec(o : Dynamic, s = '') {
		untyped {
			switch(__type__(o)){
				case "nil": return "null";
				case"number" : {
					if (o == Math.INFINITY) return "Infinity";
					else if (o == Math.NEGATIVE_INFINITY) return "-Infinity";
					else if (o != o) return "NaN";
					else return untyped tostring(o);
				}
				case "boolean" : return untyped tostring(o);
				case "string": return o;
				case "userdata": return "<userdata>";
				case "function": return "<function>";
				case "thread": return "<thread>";
				// TODO: come up with better fix for infinite recursive loop due to __class__
				case "table": { __lua__("local result = '';
		if o.toString ~= nil then result = o:toString()
		elseif o.__tostring ~= nil then result = tostring(o)
		elseif next(o) == nil then return '{}'
		else
			result = result .. '{ ';
			local first = true
			for i, v in pairs(o) do
				if i ~= '__class__' then
					if (first) then
						first = false
					else
						result = result .. ', '
					end
					result = result .. i .. ': ' .. lua.Boot.__string_rec(v, s .. 'o');
				end
			end
			result = result .. ' }';
		end");
				return result; }
				default : throw "Unknown Lua type";
		    }
		}
	};


}
