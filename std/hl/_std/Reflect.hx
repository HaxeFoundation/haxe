/*
 * Copyright (C)2005-2016 Haxe Foundation
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
		var hash = @:privateAccess field.bytes.hash();
		hl.types.Api.setField(o,hash,value);
	}

	public static function getProperty( o : Dynamic, field : String ) : Dynamic {
		var f : Dynamic = Reflect.field(o, "get_" + field);
		if( f != null )
			return f();
		return Reflect.field(o,field);
	}

	public static function setProperty( o : Dynamic, field : String, value : Dynamic ) : Void {
		var f : Dynamic = Reflect.field(o, "set_" + field);
		if( f != null )
			f(value);
		else
			setField(o, field, value);
	}

	public static function callMethod( o : Dynamic, func : haxe.Constraints.Function, args : Array<Dynamic> ) : Dynamic {
		var args : hl.types.ArrayDyn = cast args;
		var count = args.length;

		var ft = hl.types.Type.getDynamic(func);
		if( ft.kind != HFun )
			throw "Invalid function " + func;
		var need = ft.getArgsCount();
		var cval = hl.types.Api.getClosureValue(func);
		var isClosure = cval != null && need >= 0;
		if( o == null )
			o = cval;
		else if( !isClosure && count == need )
			o = null;
		var nargs = o == null ? count : count + 1;
		if( isClosure ) need++;
		if( nargs < need ) nargs = need;
		var a = new hl.types.NativeArray<Dynamic>(nargs);
		if( o == null || need < 0 ) {
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
		return [for( f in fields ) @:privateAccess String.fromUCS2(f)];
	}

	public static inline function isFunction( f : Dynamic ) : Bool {
		return hl.types.Type.getDynamic(f).kind == HFun;
	}

	@:hlNative("std","dyn_compare")
	public static function compare<T>( a : T, b : T ) : Int {
		return 0;
	}

	@:hlNative("std","fun_compare")
	public static function compareMethods( f1 : Dynamic, f2 : Dynamic ) : Bool {
		return false;
	}

	public static function isObject( v : Dynamic ) : Bool {
		var t = hl.types.Type.getDynamic(v);
		return switch( t.kind ) { case HObj, HDynObj, HVirtual: true; default: false; }
	}

	public static function isEnumValue( v : Dynamic ) : Bool {
		var t = hl.types.Type.getDynamic(v);
		return t.kind == HEnum;
	}

	public static function deleteField( o : Dynamic, field : String ) : Bool {
		return hl.types.Api.deleteField(o,@:privateAccess field.bytes.hash());
	}

	@:hlNative("std","obj_copy")
	public static function copy<T>( o : T ) : T {
		return null;
	}

	@:overload(function( f : Array<Dynamic> -> Void ) : Dynamic {})
	@:extern public inline static function makeVarArgs( f : Array<Dynamic> -> Dynamic ) : Dynamic {
		return _makeVarArgs(f);
	}

	static function _makeVarArgs( f : Array<Dynamic> -> Dynamic ) : Dynamic {
		return hl.types.Api.makeVarArgs(function(args:hl.types.NativeArray<Dynamic>) {
			var arr = hl.types.ArrayDyn.alloc(hl.types.ArrayObj.alloc(args), true);
			return f(cast arr);
		});
	}

}
