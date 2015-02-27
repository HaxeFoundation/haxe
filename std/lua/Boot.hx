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

	private static function __unhtml(s : String) {
		return s.split("&").join("&amp;").split("<").join("&lt;").split(">").join("&gt;");
	}

	private static function __trace(v,i : haxe.PosInfos) {
		untyped {
			var msg = if( i != null ) i.fileName+":"+i.lineNumber+": " else "";
			msg += __string_rec(v, "");
			if( i != null && i.customParams != null )
				for( v in i.customParams )
					msg += "," + __string_rec(v, "");
			var d;
			if( __js__("typeof")(document) != "undefined" && (d = document.getElementById("haxe:trace")) != null )
				d.innerHTML += __unhtml(msg)+"<br/>";
			else if( __js__("typeof console") != "undefined" && __js__("console").log != null )
				__js__("console").log(msg);
		}
	}

	private static function __clear_trace() {
		untyped {
			var d = document.getElementById("haxe:trace");
			if( d != null )
				d.innerHTML = "";
		}
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
	public static function defArray(tabobj: Dynamic, length : Int) : Array<Dynamic> {
		untyped __lua__("tabobj.__methods = {Array.mt};
			setmetatable(tabobj, {__index = lua.Boot.resolveMethod})");
		tabobj.length = length;
		return tabobj;
	}

	@:keep
	public static function resolveMethod(table : Dynamic,  key:Dynamic){
		untyped __lua__("for index, value in ipairs(table.__methods) do
		if value[key] ~= nil then return value[key] end
	end
	return nil");
	}

	@:ifFeature("may_print_enum")
	private static function __string_rec(o, s = '') {
		untyped {
			switch(__type__(o)){
				case "nil": return "null";
				case"number" : return untyped tostring(o);
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
