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

@:core_api class Std {
	public static function is( v : Dynamic, t : Dynamic ) : Bool {
		return untyped __global__.__instanceof(v,t);
	}

	public static function string( s : Dynamic ) : String {
		return untyped s==null ? "null" : s.toString();
	}

	public static function int( x : Float ) : Int {
		return untyped __global__.__int__(x);
	}

	public static function parseInt( x : String ) : Null<Int> {
		return untyped __global__.__hxcpp_parse_int(x);
	}

	public static function parseFloat( x : String ) : Float {
		return untyped __global__.__hxcpp_parse_float(x);
	}

	public static function random( x : Int ) : Int {
		if (x <= 0) return 0;
		return untyped __global__.__hxcpp_irand(x);
	}

	@:macro public static function format( fmt : haxe.macro.Expr.ExprOf<String> ) : haxe.macro.Expr.ExprOf<String> {
		return haxe.macro.Format.format(fmt);
	}

}
