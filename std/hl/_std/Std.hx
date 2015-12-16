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
import hl.types.ArrayObj;
import hl.types.ArrayI32;
import hl.types.ArrayF64;

@:coreApi
class Std {

	public static function random( max : Int ) : Int {
		throw "TODO:Std.random";
		return 0;
	}

	public static function is( v : Dynamic, t : Dynamic ) : Bool {
		throw "TODO:Std.is";
		return false;
	}

	public static function instance<T:{},S:T>( value : T, c : Class<S> ) : S {
		throw "TODO:Std.instance";
		return null;
	}

	public static inline function int( v : Float ) : Int {
		return untyped $int(v);
	}
	
	public static function string( v : Dynamic ) : String {
		var len = 0;
		var bytes = hl.types.Bytes.ofValue(v,new hl.types.Ref(len));
		return @:privateAccess String.__alloc__(bytes,len,bytes.utf8Length(0,len));
	}
	
	public static function parseInt( s : String ) : Null<Int> {
		return @:privateAccess s.bytes.parseInt(0, s.size);
	}

	public static function parseFloat( s : String ) : Float {
		return @:privateAccess s.bytes.parseFloat(0, s.size);
	}


}
