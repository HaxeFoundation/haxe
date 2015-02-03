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
		return untyped __define_feature__("js.Boot.isClass", o.__name__);
	}

	static inline function isEnum(e:Dynamic) : Bool {
		return untyped __define_feature__("js.Boot.isEnum", e.__ename__);
	}

	static inline function getClass(o:Dynamic) : Dynamic {
		if (Std.is(o, Array))
			return Array;
		else {
			var cl = untyped __define_feature__("js.Boot.getClass", o.__class__);
			if (cl != null)
				return cl;
			var name = __nativeClassName(o);
			if (name != null)
				return __resolveNativeClass(name);
			return null;
		}
	}

	@:ifFeature("may_print_enum")
	private static function __string_rec(o,s:String) {
		untyped {
		    // TODO: Lua impl
		    return o + "";
        }
    }

	static var __toStr = untyped __js__("{}.toString");
	// get native JS [[Class]]
	static function __nativeClassName(o:Dynamic):String {
		var name = untyped __toStr.call(o).slice(8, -1);
		// exclude general Object and Function
		// also exclude Math and JSON, because instanceof cannot be called on them
		if (name == "Object" || name == "Function" || name == "Math" || name == "JSON")
			return null;
		return name;
	}

	// check for usable native JS object
	static function __isNativeObj(o:Dynamic):Bool {
		return __nativeClassName(o) != null;
	}

	// resolve native JS class (with window or global):
	static function __resolveNativeClass(name:String) untyped {
		if (__js__("typeof window") != "undefined")
			return window[name];
		else
			return global[name];
	}

}
