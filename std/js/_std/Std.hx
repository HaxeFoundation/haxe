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
import js.Boot;

@:core_api class Std {

	public static inline function is( v : Dynamic, t : Dynamic ) : Bool {
		return untyped js.Boot.__instanceof(v,t);
	}

	public static function string( s : Dynamic ) : String {
		return untyped js.Boot.__string_rec(s,"");
	}

	public static inline function int( x : Float ) : Int {
		return cast(x) | 0;
	}

	public static function parseInt( x : String ) : Null<Int> {
		var v = untyped __js__("parseInt")(x, 10);
		// parse again if hexadecimal
		if( v == 0 && x.charCodeAt(1) == 'x'.code )
			v = untyped __js__("parseInt")(x);
		if( untyped __js__("isNaN")(v) )
			return null;
		return cast v;
	}

	public static function parseFloat( x : String ) : Float {
		return untyped __js__("parseFloat")(x);
	}

	public static function random( x : Int ) : Int {
		return untyped Math.floor(Math.random()*x);
	}

	@:macro public static function format( fmt : haxe.macro.Expr.ExprOf<String> ) : haxe.macro.Expr.ExprOf<String> {
		return haxe.macro.Context.format(fmt);
	}

	static function __init__() : Void untyped {
		String.prototype.__class__ = __feature__("Type.resolveClass",$hxClasses["String"] = String,String);
		String.__name__ = ["String"];
		Array.prototype.__class__ = __feature__("Type.resolveClass",$hxClasses["Array"] = Array,Array);
		Array.__name__ = ["Array"];
		Date.prototype.__class__ = __feature__("Type.resolveClass",$hxClasses["Date"] = Date,Date);
		Date.__name__ = ["Date"];
		var Int = __feature__("Type.resolveClass", $hxClasses["Int"] = { __name__ : ["Int"] }, { __name__ : ["Int"] });
		var Dynamic = __feature__("Type.resolveClass", $hxClasses["Dynamic"] = { __name__ : ["Dynamic"] }, { __name__ : ["Dynamic"] });
		var Float = __feature__("Type.resolveClass", $hxClasses["Float"] = __js__("Number"), __js__("Number"));
		Float.__name__ = ["Float"];
		var Bool = __feature__("Type.resolveEnum",$hxClasses["Bool"] = __js__("Boolean"), __js__("Boolean"));
		Bool.__ename__ = ["Bool"];
		var Class = __feature__("Type.resolveClass", $hxClasses["Class"] = { __name__ : ["Class"] }, { __name__ : ["Class"] });
		var Enum = {};
		var Void = __feature__("Type.resolveEnum", $hxClasses["Void"] = { __ename__ : ["Void"] }, { __ename__ : ["Void"] });
	}

}
