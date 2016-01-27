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

@:coreApi
class Reflect {

	public static function hasField( o : Dynamic, field : String ) : Bool {
		if( field == null ) return false;
		var hash = @:privateAccess field.bytes.hash();
		return hl.types.Api.hasField(o,hash);
	}

	public static function field( o : Dynamic, field : String ) : Dynamic {
		if( field == null ) return null;
		var hash = @:privateAccess field.bytes.hash();
		return hl.types.Api.getField(o,hash);
	}

	public static function setField( o : Dynamic, field : String, value : Dynamic ) : Void {
		throw "TODO";
	}

	public static function getProperty( o : Dynamic, field : String ) : Dynamic {
		throw "TODO";
		return null;
	}

	public static function setProperty( o : Dynamic, field : String, value : Dynamic ) : Void {
		throw "TODO";
	}

	public static function callMethod( o : Dynamic, func : haxe.Constraints.Function, args : Array<Dynamic> ) : Dynamic {
		var args : hl.types.ArrayDyn = cast args;
		var count = args.length;
		var nargs = o == null ? count : count + 1;
		var a = new hl.types.NativeArray<Dynamic>(nargs);
		if( o == null ) {
			for( i in 0...count )
				a[i] = args.getDyn(i);
		} else {
			func = hl.types.Api.noClosure(func);
			a[0] = o;
			for( i in 0...count )
				a[i+1] = args.getDyn(i);
		}
		return hl.types.Api.callMethod(func,a);
	}

	@:hlNative("std","obj_fields") static function getObjectFields( v : Dynamic ) : hl.types.NativeArray<hl.types.Bytes> {
		return null;
	}

	public static function fields( o : Dynamic ) : Array<String> {
		var fields = getObjectFields(o);
		if( fields == null ) return [];
		return [for( f in fields ) @:privateAccess String.__alloc__(f,f.ucs2Length(0))];
	}

	public static inline function isFunction( f : Dynamic ) : Bool {
		return hl.types.Type.getDynamic(f).kind == HFun;
	}

	@:hlNative("std","dyn_compare")
	public static function compare<T>( a : T, b : T ) : Int {
		return 0;
	}

	public static function compareMethods( f1 : Dynamic, f2 : Dynamic ) : Bool {
		throw "TODO";
		return false;
	}

	public static function isObject( v : Dynamic ) : Bool {
		throw "TODO";
		return false;
	}

	public static function isEnumValue( v : Dynamic ) : Bool {
		throw "TODO";
		return false;
	}

	public static function deleteField( o : Dynamic, field : String ) : Bool {
		return hl.types.Api.deleteField(o,@:privateAccess field.bytes.hash());
	}

	public static function copy<T>( o : T ) : T {
		throw "TODO";
		return null;
	}

	@:overload(function( f : Array<Dynamic> -> Void ) : Dynamic {})
	public static function makeVarArgs( f : Array<Dynamic> -> Dynamic ) : Dynamic {
		throw "TODO";
		return null;
	}

}
