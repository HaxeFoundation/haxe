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

	@:feature("typed_cast")
	public static function is( v : Dynamic, t : Dynamic ) : Bool {
		return untyped neko.Boot.__instanceof(v,t);
	}

	public static function string( s : Dynamic ) : String {
		return new String(untyped __dollar__string(s));
	}

	public static function int( x : Float ) : Int {
		if( x < 0 ) return Math.ceil(x);
		return Math.floor(x);
	}

	public static function parseInt( x : String ) : Null<Int> untyped {
		var t = __dollar__typeof(x);
		if( t == __dollar__tint )
			return x;
		if( t == __dollar__tfloat )
			return __dollar__int(x);
		if( t != __dollar__tobject )
			return null;
		return __dollar__int(x.__s);
	}

	public static function parseFloat( x : String ) : Float untyped {
		if( x == null ) return Math.NaN;
		var t = __dollar__float(x.__s);
		if( t == null ) t = Math.NaN;
		return t;
	}

	public static function random( x : Int ) : Int {
		return untyped Math._rand_int(Math.__rnd,x);
	}

	@:macro public static function format( fmt : haxe.macro.Expr.ExprOf<String> ) : haxe.macro.Expr.ExprOf<String> {
		return haxe.macro.Format.format(fmt);
	}

	static function __init__() : Void untyped {
		Int = { __name__ : ["Int"] };
		Float = { __name__ : ["Float"] };
		Bool = { __ename__ : ["Bool"] };
		Dynamic = { __name__ : ["Dynamic"] };
		Class = { __name__ : ["Class"] };
		Enum = {};
		var cl = neko.Boot.__classes;
		cl.Int = Int;
		cl.Float = Float;
		cl.Bool = Bool;
		cl.Dynamic = Dynamic;
		cl.Class = Class;
		cl.Enum = Enum;
	}

}
