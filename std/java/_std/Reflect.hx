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
import java.internal.Function;
import java.Boot;
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

/**
	The Reflect API is a way to manipulate values dynamicly through an
	abstract interface in an untyped manner. Use with care.
**/
@:keep @:coreApi class Reflect {

	/**
		Tells if an object has a field set. This doesn't take into account the object prototype (class methods).
	**/
	@:functionCode('
		if (o instanceof haxe.lang.IHxObject)
		return ((haxe.lang.IHxObject) o).__hx_getField(field, false, true, false) != haxe.lang.Runtime.undefined;

		return haxe.lang.Runtime.slowHasField(o, field);
	')
	public static function hasField( o : Dynamic, field : String ) : Bool
	{
		return false;
	}

	/**
		Returns the field of an object, or null if [o] is not an object or doesn't have this field.
	**/
	@:functionCode('
		if (o instanceof haxe.lang.IHxObject)
			return ((haxe.lang.IHxObject) o).__hx_getField(field, false, false, false);

		return haxe.lang.Runtime.slowGetField(o, field, false);
	')
	public static function field( o : Dynamic, field : String ) : Dynamic
	{
		return null;
	}


	/**
		Set an object field value.
	**/
	@:functionCode('
		if (o instanceof haxe.lang.IHxObject)
			((haxe.lang.IHxObject) o).__hx_setField(field, value, false);
		else
			haxe.lang.Runtime.slowSetField(o, field, value);
	')
	public static function setField( o : Dynamic, field : String, value : Dynamic ) : Void
	{

	}

	/**
		Similar to field but also supports property (might be slower).
	**/
	@:functionCode('
		if (o instanceof haxe.lang.IHxObject)
			return ((haxe.lang.IHxObject) o).__hx_getField(field, false, false, true);

		if (haxe.lang.Runtime.slowHasField(o, "get_" + field))
			return haxe.lang.Runtime.slowCallField(o, "get_" + field, null);

		return haxe.lang.Runtime.slowGetField(o, field, false);
	')
	public static function getProperty( o : Dynamic, field : String ) : Dynamic
	{
		return null;
	}

	/**
		Similar to setField but also supports property (might be slower).
	**/
	@:functionCode('
		if (o instanceof haxe.lang.IHxObject)
			((haxe.lang.IHxObject) o).__hx_setField(field, value, true);
		else if (haxe.lang.Runtime.slowHasField(o, "set_" + field))
			haxe.lang.Runtime.slowCallField(o, "set_" + field, new Array( new java.lang.Object[]{value} ));
		else
			haxe.lang.Runtime.slowSetField(o, field, value);
	')
	public static function setProperty( o : Dynamic, field : String, value : Dynamic ) : Void
	{

	}

	/**
		Call a method with the given object and arguments.
	**/
	@:functionCode('
		return ((haxe.lang.Function) func).__hx_invokeDynamic(args);
	')
	public static function callMethod( o : Dynamic, func : Dynamic, args : Array<Dynamic> ) : Dynamic
	{
		return null;
	}

	/**
		Returns the list of fields of an object, excluding its prototype (class methods).
	**/
	@:functionCode('
		if (o instanceof haxe.lang.IHxObject)
		{
			Array<String> ret = new Array<String>();
				((haxe.lang.IHxObject) o).__hx_getFields(ret);
			return ret;
		} else if (o instanceof java.lang.Class) {
			return Type.getClassFields( (java.lang.Class) o);
		} else {
			return new Array<String>();
		}
	')
	public static function fields( o : Dynamic ) : Array<String>
	{
		return null;
	}

	/**
		Tells if a value is a function or not.
	**/
	@:functionCode('
		return f instanceof haxe.lang.Function;
	')
	public static function isFunction( f : Dynamic ) : Bool
	{
		return false;
	}

	/**
		Generic comparison function, does not work for methods, see [compareMethods]
	**/
	@:functionCode('
		return haxe.lang.Runtime.compare(a, b);
	')
	public static function compare<T>( a : T, b : T ) : Int
	{
		return 0;
	}

	/**
		Compare two methods closures. Returns true if it's the same method of the same instance.
	**/
	@:functionCode('
		if (f1 == f2)
			return true;

		if (f1 instanceof haxe.lang.Closure && f2 instanceof haxe.lang.Closure)
		{
			haxe.lang.Closure f1c = (haxe.lang.Closure) f1;
			haxe.lang.Closure f2c = (haxe.lang.Closure) f2;

			return haxe.lang.Runtime.refEq(f1c.obj, f2c.obj) && f1c.field.equals(f2c.field);
		}


		return false;
	')
	public static function compareMethods( f1 : Dynamic, f2 : Dynamic ) : Bool
	{
		return false;
	}

	/**
		Tells if a value is an object or not.

	**/
	@:functionCode('
		return v instanceof haxe.lang.DynamicObject;
	')
	public static function isObject( v : Dynamic ) : Bool
	{
		return false;
	}

	/**
		Delete an object field.
	**/
	@:functionCode('
		return (o instanceof haxe.lang.DynamicObject && ((haxe.lang.DynamicObject) o).__hx_deleteField(f));
	')
	public static function deleteField( o : Dynamic, field : String ) : Bool
	{
		return false;
	}

	/**
		Make a copy of the fields of an object.
	**/
	public static function copy<T>( o : T ) : T
	{
		var o2 : Dynamic = {};
		for( f in Reflect.fields(o) )
			Reflect.setField(o2,f,Reflect.field(o,f));
		return cast o2;
	}

	/**
		Transform a function taking an array of arguments into a function that can
		be called with any number of arguments.
	**/
	@:overload(function( f : Array<Dynamic> -> Void ) : Dynamic {})
	public static function makeVarArgs( f : Array<Dynamic> -> Dynamic ) : Dynamic
	{
		return new VarArgsFunction(f);
	}


}
