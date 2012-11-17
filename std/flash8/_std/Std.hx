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
@:coreApi class Std {

	@:ifFeature("typed_cast")
	public static function is( v : Dynamic, t : Dynamic ) : Bool {
		return untyped flash.Boot.__instanceof(v,t);
	}

	public static function string( s : Dynamic ) : String {
		return untyped flash.Boot.__string_rec(s,"");
	}

	public static function int( x : Float ) : Int {
		if( x < 0 ) return Math.ceil(x);
		return Math.floor(x);
	}

	public static function parseInt( x : String ) : Null<Int> untyped {
		var v;
		if( x.charCodeAt(1) == 'x'.code || x.charCodeAt(1) == 'X'.code)
			v = _global["parseInt"](x);
		else
			v = _global["parseInt"](x, 10);
		if( _global["isNaN"](v) )
			return null;
		return v;
	}

	public static function parseFloat( x : String ) : Float {
		return untyped _global["parseFloat"](x);
	}

	public static function random( x : Int ) : Int {
		return untyped __random__(x);
	}

	#if !haxe3
	@:macro public static function format( fmt : haxe.macro.Expr.ExprOf<String> ) : haxe.macro.Expr.ExprOf<String> {
		return haxe.macro.Format.format(fmt);
	}
	#end

	static function __init__() : Void untyped {
		var g : Dynamic = _global;
		g["Int"] = { __name__ : ["Int"] };
		g["Bool"] = { __ename__ : ["Bool"] };
		g.Dynamic = { __name__ : [__unprotect__("Dynamic")] };
		g.Class = { __name__ : [__unprotect__("Class")] };
		g.Enum = {};
		g.Void = { __ename__ : [__unprotect__("Void")] };
		g["Float"] = _global["Number"];
		g["Float"][__unprotect__("__name__")] = ["Float"];
		Array.prototype[__unprotect__("__class__")] = Array;
		Array[__unprotect__("__name__")] = ["Array"];
		String.prototype[__unprotect__("__class__")] = String;
		String[__unprotect__("__name__")] = ["String"];
		g["ASSetPropFlags"](Array.prototype,null,7);
	}

}
