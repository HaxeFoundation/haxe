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

class Boot {
	public static var unpack : Dynamic->lua.Table<Int,Dynamic> = untyped __lua__("function(...) return {...} end");

	static function __unhtml(s : String) {
		return s.split("&").join("&amp;").split("<").join("&lt;").split(">").join("&gt;");
	}

	static inline function isClass(o:Dynamic) : Bool {
		return untyped __define_feature__("lua.Boot.isClass", o.__name__);
	}

	static inline function isEnum(e:Dynamic) : Bool {
		return untyped __define_feature__("lua.Boot.isEnum", e.__ename__);
	}

	static inline function getClass(o:Dynamic) : Dynamic {
	    return null;
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
			__index : __lua__("Array.mt"),
			__newindex : lua.Boot.arrayNewIndex
		});
		return tabobj;
	}

	@:keep
	public static function resolveMethod(table : Dynamic,  key:Dynamic){
		untyped __lua__("for index, value in ipairs(table.__methods) do
		if value[key] ~= nil then return value[key] end
	end
	return nil");
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
				case "table": { __lua__("local result = '';
		if o.toString ~= nil then result = o:toString()
		elseif o.__tostring ~= nil then result = tostring(o)
		elseif next(o) == nil then return '{}'
		else
			result = result .. '{ ';
			local first = true
			for i, v in pairs(o) do
				if (first) then first = false
				else result = result .. ','
				end
				result = result .. i .. ' => ' .. lua.Boot.__string_rec(v, s .. 'o');
			end
			result = result .. ' }';
		end");
				return result; }
				default : throw "Unknown Lua type";
		    }
		}
	};


}
