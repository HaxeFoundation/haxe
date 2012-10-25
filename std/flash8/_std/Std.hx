/*
 * Copyright (c) 2005, The haXe Project Contributors
 * All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   - Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   - Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE HAXE PROJECT CONTRIBUTORS "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE HAXE PROJECT CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
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

	@:macro public static function format( fmt : haxe.macro.Expr.ExprOf<String> ) : haxe.macro.Expr.ExprOf<String> {
		return haxe.macro.Format.format(fmt);
	}

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
