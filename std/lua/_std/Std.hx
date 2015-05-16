/*
 * Copyright (C)2005-2015 Haxe Foundation
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
import lua.Boot;

@:keepInit
@:coreApi class Std {

	public static inline function is( v : Dynamic, t : Dynamic ) : Bool {
		return untyped lua.Boot.__instanceof(v,t);
	}

	public static inline function instance<T:{},S:T>( value : T, c : Class<S> ) : S {
		return untyped __instanceof__(value, c) ? cast value : null;
	}

	@:keep
	inline public static function string( s : Dynamic ) : String {
		return untyped lua.Boot.__string_rec(s,"");
	}

	public static inline function int( x : Float ) : Int {
		return Math.floor(x + .5); 
	}

	public static function parseInt( x : String ) : Null<Int> {
		var v = null;
		if( v == 0 && (x.charCodeAt(1) == 'x'.code || x.charCodeAt(1) == 'X'.code) )
			v = Std.int(untyped tonumber(x,16)); 
		else 
			v = Std.int(untyped tonumber(x)); 
		if(Math.isNaN(v)) return null;
		return cast v;
	}

	public static inline function parseFloat( x : String ) : Float {
		return untyped tonumber(x); 
	}

	public static function random( x : Int ) : Int {
		return untyped x <= 0 ? 0 : Math.floor(Math.random()*x);
	}

	static function __init__() : Void untyped {
		__feature__("js.Boot.getClass",String.prototype.__class__ = __feature__("Type.resolveClass",_hxClasses["String"] = String,String));
		__feature__("js.Boot.isClass",String.__name__ = __feature__("Type.getClassName",["String"],true));
		__feature__("Type.resolveClass",_hxClasses["Array"] = Array);
		__feature__("js.Boot.isClass",Array.__name__ = __feature__("Type.getClassName",["Array"],true));
		__feature__("Date.*", {
			var Date = {};
			__feature__("js.Boot.getClass", _hxClasses["Date"] = {}, {});
			__feature__("js.Boot.isClass", Date.__name__ = ["Date"]);
		});
		__feature__("Int.*",{
			var Int = __feature__("Type.resolveClass", _hxClasses["Int"] = { __name__ : ["Int"] }, { __name__ : ["Int"] });
		});
		__feature__("Dynamic.*",{
			var Dynamic = __feature__("Type.resolveClass", _hxClasses["Dynamic"] = { __name__ : ["Dynamic"] }, { __name__ : ["Dynamic"] });
		});
		__feature__("Float.*",{
			var Float = __feature__("Type.resolveClass", _hxClasses["Float"]={}, {});
			Float.__name__ = ["Float"];
		});
		__feature__("Bool.*",{
			var Bool = __feature__("Type.resolveEnum",_hxClasses["Bool"] = {}, {});
			Bool.__ename__ = ["Bool"];
		});
		__feature__("Class.*",{
			var Class = __feature__("Type.resolveClass", _hxClasses["Class"] = { __name__ : ["Class"] }, { __name__ : ["Class"] });
		});
		__feature__("Enum.*",{
			var Enum = {};
		});
		__feature__("Void.*",{
			var Void = __feature__("Type.resolveEnum", _hxClasses["Void"] = { __ename__ : ["Void"] }, { __ename__ : ["Void"] });
		});

	}

}
