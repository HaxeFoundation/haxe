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

	public static function is( v : Dynamic, t : Dynamic ) : Bool {
		return untyped untyped __call__("_hx_instanceof", v,t);
	}

	public static function string( s : Dynamic ) : String {
		return untyped __call__("_hx_string_rec", s, '');
	}

	public inline static function int( x : Float ) : Int {
		return untyped __call__("intval", x);
	}

	public static function parseInt( x : String ) : Null<Int> {
		untyped if (!__call__("is_numeric", x)) {
			var matches = null;
			__call__('preg_match', '/^-?\\d+/', x, matches);
			return __call__("count", matches) == 0 ? null : __call__('intval', matches[0]);
		} else
			return x.substr(0, 2).toLowerCase() == "0x" ? __php__("(int) hexdec(substr($x, 2))") : __php__("intval($x)");
	}

	public static function parseFloat( x : String ) : Float {
		var v : Float = untyped __call__("floatval", x);
		untyped	if (v==0.0) {
			x=untyped __call__("rtrim", x);
			v=untyped __call__("floatval", x);
			if (v == 0.0 && !__call__("is_numeric", x)) v = untyped __call__("acos", 1.01);
		}
		return v;
	}

	public static function random( x : Int ) : Int {
		return untyped x <= 0 ? 0 : __call__("mt_rand", 0, x-1);
	}

	@:macro public static function format( fmt : haxe.macro.Expr.ExprOf<String> ) : haxe.macro.Expr.ExprOf<String> {
		return haxe.macro.Context.format(fmt);
	}
}
