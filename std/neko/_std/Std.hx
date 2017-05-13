/*
 * Copyright (C)2005-2017 Haxe Foundation
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
		return untyped neko.Boot.__instanceof(v,t);
	}

	public static function instance<T:{},S:T>( value : T, c : Class<S> ) : S {
		return Std.is(value, c) ? cast value : null;
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
