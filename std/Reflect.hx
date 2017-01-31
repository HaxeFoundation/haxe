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
/**
	The Reflect API is a way to manipulate values dynamically through an
	abstract interface in an untyped manner. Use with care.

	@see https://haxe.org/manual/std-reflection.html
**/
extern class Reflect {

	/**
		Tells if structure `o` has a field named `field`.

		This is only guaranteed to work for anonymous structures. Refer to
		`Type.getInstanceFields` for a function supporting class instances.

		If `o` or `field` are null, the result is unspecified.
	**/
	public static function hasField( o : Dynamic, field : String ) : Bool;

	/**
		Returns the value of the field named `field` on object `o`.

		If `o` is not an object or has no field named `field`, the result is
		null.

		If the field is defined as a property, its accessors are ignored. Refer
		to `Reflect.getProperty` for a function supporting property accessors.

		If `field` is null, the result is unspecified.

		(As3) If used on a property field, the getter will be invoked. It is
		not possible to obtain the value directly.
	**/
	public static function field( o : Dynamic, field : String ) : Dynamic;

	/**
		Sets the field named `field` of object `o` to value `value`.

		If `o` has no field named `field`, this function is only guaranteed to
		work for anonymous structures.

		If `o` or `field` are null, the result is unspecified.

		(As3) If used on a property field, the setter will be invoked. It is
		not possible to set the value directly.
	**/
	public static function setField( o : Dynamic, field : String, value : Dynamic ) : Void;

	/**
		Returns the value of the field named `field` on object `o`, taking
		property getter functions into account.

		If the field is not a property, this function behaves like
		`Reflect.field`, but might be slower.

		If `o` or `field` are null, the result is unspecified.
	**/
	public static function getProperty( o : Dynamic, field : String ) : Dynamic;

	/**
		Sets the field named `field` of object `o` to value `value`, taking
		property setter functions into account.

		If the field is not a property, this function behaves like
		`Reflect.setField`, but might be slower.

		If `field` is null, the result is unspecified.
	**/
	public static function setProperty( o : Dynamic, field : String, value : Dynamic ) : Void;

	/**
		Call a method with the given object and arguments.
	**/
	public static function callMethod( o : Dynamic, func : haxe.Constraints.Function, args : Array<Dynamic> ) : Dynamic;

	/**
		Returns the fields of structure `o`.

		This method is only guaranteed to work on anonymous structures. Refer to
		`Type.getInstanceFields` for a function supporting class instances.

		If `o` is null, the result is unspecified.
	**/
	public static function fields( o : Dynamic ) : Array<String>;

	/**
		Returns true if `f` is a function, false otherwise.

		If `f` is null, the result is false.
	**/
	public static function isFunction( f : Dynamic ) : Bool;

	/**
		Compares `a` and `b`.

		If `a` is less than `b`, the result is negative. If `b` is less than
		`a`, the result is positive. If `a` and `b` are equal, the result is 0.

		This function is only defined if `a` and `b` are of the same type.

		If that type is a function, the result is unspecified and
		`Reflect.compareMethods` should be used instead.

		For all other types, the result is 0 if `a` and `b` are equal. If they
		are not equal, the result depends on the type and is negative if:

		- Numeric types: a is less than b
		- String: a is lexicographically less than b
		- Other: unspecified

		If `a` and `b` are null, the result is 0. If only one of them is null,
		the result is unspecified.
	**/
	public static function compare<T>( a : T, b : T ) : Int;

	/**
		Compares the functions `f1` and `f2`.

		If `f1` or `f2` are not functions, the result is unspecified.

		Otherwise the result is true if `f1` and the `f2` are physically equal,
		false otherwise.
	**/
	public static function compareMethods( f1 : Dynamic, f2 : Dynamic ) : Bool;

	/**
		Tells if `v` is an object.

		The result is true if `v` is one of the following:

		- class instance
		- structure
		- `Class<T>`
		- `Enum<T>`

		Otherwise, including if `v` is null, the result is false.
	**/
	public static function isObject( v : Dynamic ) : Bool;

	/**
		Tells if `v` is an enum value.

		The result is true if `v` is of type EnumValue, i.e. an enum
		constructor.

		Otherwise, including if `v` is null, the result is false.
	**/
	public static function isEnumValue( v : Dynamic ) : Bool;

	/**
		Removes the field named `field` from structure `o`.

		This method is only guaranteed to work on anonymous structures.

		If `o` or `field` are null, the result is unspecified.
	**/
	public static function deleteField( o : Dynamic, field : String ) : Bool;

	/**
		Copies the fields of structure `o`.

		This is only guaranteed to work on anonymous structures.

		If `o` is null, the result is unspecified.
	**/
	public static function copy<T>( o : T ) : T;

	/**
		Transform a function taking an array of arguments into a function that can
		be called with any number of arguments.
	**/
	@:overload(function( f : Array<Dynamic> -> Void ) : Dynamic {})
	public static function makeVarArgs( f : Array<Dynamic> -> Dynamic ) : Dynamic;

}
