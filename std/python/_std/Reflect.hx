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

import python.internal.AnonObject;
import python.internal.StringImpl;
import python.internal.ArrayImpl;

import python.lib.Builtin;
import python.lib.Inspect;
import python.Syntax;
import python.VarArgs;

@:access(python.Boot)
@:coreApi
class Reflect {


	static inline function handleKeywords(name:String):String {
		return python.Boot.handleKeywords(name);
	}

	static function unhandleKeywords(name:String):String {
		return python.Boot.unhandleKeywords(name);
	}

	public static function hasField( o : Dynamic, field : String ) : Bool
	{
		var field = handleKeywords(field);
		return Builtin.hasattr(o, field);
	}

	static inline function isString (o:Dynamic):Bool {
		return Builtin.isinstance(o, String);
	}
	static inline function isArray (o:Dynamic):Bool {
		return Builtin.isinstance(o, Array);
	}
	@:access(python.Boot)
	@:keep public static function field( o : Dynamic, field : String ) : Dynamic
	{
		return python.Boot.field(o, field);
	}

	@:keep public static function setField( o : Dynamic, field : String, value : Dynamic ) : Void
	{
		var field = handleKeywords(field);
		return Builtin.setattr(o,field,value);
	}

	public static function getProperty( o : Dynamic, field : String ) : Dynamic
	{
		var field = handleKeywords(field);
		var tmp = null;
		if (o == null) {
			return null;
		} else {
			tmp = Reflect.field(o, "get_" + field);
			if (tmp != null && Builtin.callable(tmp)) {
				return tmp();
			} else {
				return Reflect.field(o, field);
			}
		}
	}

	public static function setProperty( o : Dynamic, field : String, value : Dynamic ) : Void {

		var field = handleKeywords(field);

		return if (Builtin.hasattr(o,"set_"+field)) {
			var tmp = Builtin.getattr(o,"set_"+field);
			tmp(value);
		}
		else Builtin.setattr(o,field, value);
	}

	public static function callMethod( o : Dynamic, func : haxe.Constraints.Function, args : Array<Dynamic> ) : Dynamic
	{
		var args:VarArgs = args;
		return if (Builtin.callable(func)) func(python.Syntax.varArgs(args)) else null;
	}

	public static inline function fields( o : Dynamic ) : Array<String>
	{
		return python.Boot.fields(o);
	}

	public static function isFunction( f : Dynamic ) : Bool
	{
		return Inspect.isfunction(f) || Inspect.ismethod(f);
	}

	public static function compare<T>( a : T, b : T ) : Int {
		if (a == null && b == null) return 0;
		return
			if (a == null) 1 else if (b == null) -1
			else ( a == b ) ? 0 : (((cast a) > (cast b)) ? 1 : -1);
	}

	public static function compareMethods( f1 : Dynamic, f2 : Dynamic ) : Bool {
		if( f1 == f2 )
			return true;
		if( !isFunction(f1) || !isFunction(f2) )
			return false;

		return false;
	}

	public static function isObject( v : Dynamic ) : Bool {

		return switch (Type.typeof(v)) {
			case TObject, TClass(_): true;
			case _ : false;
		}
	}

	public static function isEnumValue( v : Dynamic ) : Bool {
		return v != Enum && Builtin.isinstance(v, cast Enum);
	}

	public static function deleteField( o : Dynamic, field : String ) : Bool {
		if( !hasField(o,field) ) return false;
		Syntax.callField(o, "__delattr__", field);
		return true;
	}

	public static function copy<T>( o : T ) : T {
		var o2 : Dynamic = {};
		for( f in Reflect.fields(o) )
			Reflect.setField(o2,f,Reflect.field(o,f));
		return o2;
	}



	@:overload(function( f : Array<Dynamic> -> Void ) : Dynamic {})
	public static function makeVarArgs( f : Array<Dynamic> -> Dynamic ) : Dynamic {
		return function (v:VarArgs) {
			return f(v);
		}
	}

}