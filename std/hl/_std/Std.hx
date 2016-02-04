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
import hl.Boot;

@:coreApi
class Std {

	@:hlNative("std","random")
	public static function random( x : Int ) : Int {
		return 0;
	}

	@:extern public inline static function is( v : Dynamic, t : Dynamic ) : Bool {
		return untyped $is(v,t);
	}

	@:extern public inline static function instance<T:{},S:T>( value : T, c : Class<S> ) : S {
		return untyped $instance(value,c);
	}

	@:extern public static inline function int( x : Float ) : Int {
		return untyped $int(x);
	}

	@:keep public static function string( s : Dynamic ) : String {
		var len = 0;
		var bytes = hl.types.Bytes.ofValue(s,new hl.types.Ref(len));
		return @:privateAccess String.__alloc__(bytes,len>>1);
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
		var ta = hl.types.Type.getDynamic(a);
		var tb = hl.types.Type.getDynamic(b);
		if( ta == hl.types.Type.get("") )
			return (a : String) + b;
		if( tb == hl.types.Type.get("") )
			return a + (b : String);
		switch( (cast ta.kind : Int) | ((cast tb.kind : Int) << 8) ) {
		case 0x0303:
			return (a:Int) + (b : Int);
		case x:
			throw "Can't add "+ta+" and "+tb+" ("+StringTools.hex(x)+")";
		}
		return null;
	}

}
