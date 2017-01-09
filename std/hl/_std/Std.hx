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
import hl.Boot;

private typedef Rand = hl.Abstract<"hl_random">;

@:coreApi
class Std {

	static var rnd : Rand;

	static function __init__() : Void {
		rnd = rnd_sys();
	}

	@:hlNative("std","rnd_init_system") static function rnd_sys() : Rand { return null; }
	@:hlNative("std","rnd_int") static function rnd_int( r : Rand ) : Int { return 0; }
	@:hlNative("std","rnd_float") static function rnd_float( r : Rand ) : Float { return 0.; }

	public static function random( x : Int ) : Int {
		return x <= 0 ? 0 : (rnd_int(rnd) & 0x3FFFFFFF) % x;
	}

	public static function is( v : Dynamic, t : Dynamic ) : Bool {
		var t : hl.BaseType = t;
		if( t == null ) return false;
		switch( t.__type__.kind ) {
		case HDyn:
			return true;
		case HF64:
			switch( hl.Type.getDynamic(v).kind ) {
			case HUI8, HUI16, HI32:
				return true;
			default:
			}
		case HI32:
			switch( hl.Type.getDynamic(v).kind ) {
			case HF32, HF64:
				var v : Float = v;
				return Std.int(v) == v;
			default:
			}
		default:
		}
		return t.check(v);
	}

	@:extern public inline static function instance<T:{},S:T>( value : T, c : Class<S> ) : S {
		var t : hl.BaseType = cast c;
		return t.check(value) ? cast value : null;
	}

	@:extern public static inline function int( x : Float ) : Int {
		return untyped $int(x);
	}

	@:keep public static function string( s : Dynamic ) : String {
		var len = 0;
		var bytes = hl.Bytes.fromValue(s,new hl.Ref(len));
		return @:privateAccess String.__alloc__(bytes,len);
	}

	public static function parseInt( x : String ) : Null<Int> {
		if( x == null ) return null;
		return @:privateAccess x.bytes.parseInt(0, x.length<<1);
	}

	public static function parseFloat( x : String ) : Float {
		if( x == null ) return Math.NaN;
		return @:privateAccess x.bytes.parseFloat(0, x.length<<1);
	}

	@:keep static function __add__( a : Dynamic, b : Dynamic ) : Dynamic {
		var ta = hl.Type.getDynamic(a);
		var tb = hl.Type.getDynamic(b);
		if( ta == hl.Type.get("") )
			return (a : String) + b;
		if( tb == hl.Type.get("") )
			return a + (b : String);
		switch(ta.kind) {
		case HUI8, HUI16, HI32:
			var a : Int = a;
			switch( tb.kind ) {
			case HUI8, HUI16, HI32: return a + (b:Int);
			case HF32, HF64: return a + (b:Float);
			default:
			}
		case HF32, HF64:
			var a : Float = a;
			switch( tb.kind ) {
			case HUI8, HUI16, HI32: return a + (b:Int);
			case HF32, HF64: return a + (b:Float);
			default:
			}
		default:
		}
		throw "Can't add "+a+"("+ta+") and "+b+"("+tb+")";
		return null;
	}

}
