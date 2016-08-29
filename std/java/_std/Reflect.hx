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

import java.internal.Function;
import java.internal.HxObject;
import java.internal.Runtime;
import java.Boot;

@:coreApi class Reflect {

	public static function hasField( o : Dynamic, field : String ) : Bool
	{
		if (Std.is(o, IHxObject)) {
			return untyped (o : IHxObject).__hx_getField(field, false, true, false) != Runtime.undefined;
		}
		return Runtime.slowHasField(o, field);
	}

	@:keep
	public static function field( o : Dynamic, field : String ) : Dynamic
	{
		if (Std.is(o, IHxObject)) {
			return untyped (o : IHxObject).__hx_getField(field, false, false, false);
		}
		return Runtime.slowGetField(o, field, false);
	}

	@:keep
	public static function setField( o : Dynamic, field : String, value : Dynamic ) : Void
	{
		if (Std.is(o, IHxObject)) {
			untyped (o : IHxObject).__hx_setField(field, value, false);
		} else {
			Runtime.slowSetField(o, field, value);
		}
	}

	public static function getProperty( o : Dynamic, field : String ) : Dynamic
	{
		if (Std.is(o, IHxObject)) {
			return untyped (o : IHxObject).__hx_getField(field, false, false, true);
		}
		if (Runtime.slowHasField(o, "get_" + field)) {
			return Runtime.slowCallField(o, "get_" + field, null);
		}
		return Runtime.slowGetField(o, field, false);
	}

	public static function setProperty( o : Dynamic, field : String, value : Dynamic ) : Void
	{
		if (Std.is(o, IHxObject)) {
			untyped (o : IHxObject).__hx_setField(field, value, true);
		} else if (Runtime.slowHasField(o, "set_" + field)) {
			Runtime.slowCallField(o, "set_" + field, [value]);
		} else {
			Runtime.slowSetField(o, field, value);
		}
	}

	public static function callMethod( o : Dynamic, func : haxe.Constraints.Function, args : Array<Dynamic> ) : Dynamic
	{
		return untyped (func : Function).__hx_invokeDynamic(args);
	}

	@:keep
	public static function fields( o : Dynamic ) : Array<String>
	{
		if (Std.is(o, IHxObject)) {
			var ret:Array<String> = [];
			untyped (o : IHxObject).__hx_getFields(ret);
			return ret;
		} else if (Std.is(o, java.lang.Class)) {
			return Type.getClassFields(cast o);
		} else {
			return [];
		}
	}

	public static function isFunction( f : Dynamic ) : Bool
	{
		return Std.is(f, Function);
	}

	public static function compare<T>( a : T, b : T ) : Int
	{
		return Runtime.compare(a, b);
	}

	@:access(java.internal.Closure)
	public static function compareMethods( f1 : Dynamic, f2 : Dynamic ) : Bool
	{
		if (f1 == f2) {
			return true;
		}
		if (Std.is(f1, Closure) && Std.is(f2, Closure)) {
			var f1c:Closure = cast f1;
			var f2c:Closure = cast f2;
			return Runtime.refEq(f1c.obj, f2c.obj) && f1c.field == f2c.field;
		}
		return false;
	}

	public static function isObject( v : Dynamic ) : Bool
	{
		return v != null && !(Std.is(v, HxEnum) || Std.is(v, Function) || Std.is(v, java.lang.Enum) || Std.is(v, java.lang.Number) || Std.is(v, java.lang.Boolean.BooleanClass));
	}

	public static function isEnumValue( v : Dynamic ) : Bool {
		return v != null && (Std.is(v, HxEnum) || Std.is(v, java.lang.Enum));
	}

	public static function deleteField( o : Dynamic, field : String ) : Bool
	{
		return (Std.is(o, DynamicObject) && (o : DynamicObject).__hx_deleteField(field));
	}

	public static function copy<T>( o : T ) : T
	{
		var o2 : Dynamic = {};
		for( f in Reflect.fields(o) )
			Reflect.setField(o2,f,Reflect.field(o,f));
		return cast o2;
	}

	@:overload(function( f : Array<Dynamic> -> Void ) : Dynamic {})
	public static function makeVarArgs( f : Array<Dynamic> -> Dynamic ) : Dynamic
	{
		return new VarArgsFunction(f);
	}


}
