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
/**
	The Reflect API is a way to manipulate values dynamicly through an
	abstract interface in an untyped manner. Use with care.
**/
extern class Reflect {

	/**
		Tells if structure [o] has a field named [field].
		
		This is only guaranteed to work for anonymous structures. Refer to
		[Type.getInstanceFields()] for a function supporting class instances.
		
		If [o] or [field] are null, the result is unspecified.
	**/
	public static function hasField( o : Dynamic, field : String ) : Bool;

	/**
		Returns the value of the field named [field] on object [o].
		
		If [o] is not an object or has no field named [field], the result is
		null.
		
		If the field is defined as a property, its accessors are ignored. Refer
		to [Reflect.getProperty()] for a function supporting property accessors.
		
		If [field] is null, the result is unspecified.
	**/
	public static function field( o : Dynamic, field : String ) : Dynamic;

	/**
		Sets the field named [field] of object [o] to value [value].
		
		If [o] has no field named [field], this function is only guaranteed to
		work for anonymous structures.
		
		If [o] or [field] are null, the result is unspecified.
	**/
	public static function setField( o : Dynamic, field : String, value : Dynamic ) : Void;

	/**
		Returns the value of the field named [field] on object [o], taking
		property getter functions into account.
		
		If the field is not a property, this function behaves like
		[Reflect.field], but might be slower.
		
		If [o] or [field] are null, the result is unspecified.
	**/
	public static function getProperty( o : Dynamic, field : String ) : Dynamic;

	/**
		Sets the field named [field] of object [o] to value [value], taking
		property setter functions into account.
		
		If the field is not a property, this function behaves like
		[Reflect.setField], but might be slower.
		
		If [field] is null, the result is unspecified.
	**/
	public static function setProperty( o : Dynamic, field : String, value : Dynamic ) : Void;

	/**
		Call a method with the given object and arguments.
	**/
	public static function callMethod( o : Dynamic, func : Dynamic, args : Array<Dynamic> ) : Dynamic;

	/**
		Returns the fields of structure [o].
		
		This method is only guaranteed to work on anonymous structures. Refer to
		[Type.getInstanceFields()] for a function supporting class instances.
		
		If [o] is null, the result is unspecified.
	**/
	public static function fields( o : Dynamic ) : Array<String>;

	/**
		Returns true if [f] is a function, false otherwise.
		
		If [f] is null, the result is false.
	**/
	public static function isFunction( f : Dynamic ) : Bool;

	/**
		Generic comparison function, does not work for methods, see [compareMethods]
	**/
	public static function compare<T>( a : T, b : T ) : Int;

	/**
		Compare two methods closures. Returns true if it's the same method of the same instance.
	**/
	public static function compareMethods( f1 : Dynamic, f2 : Dynamic ) : Bool;

	/**
		Tells if a value is an object or not.
	**/
	public static function isObject( v : Dynamic ) : Bool;

	/**
		Tells if [v] is an enum value.
	**/
	public static function isEnumValue( v : Dynamic ) : Bool;
	
	/**
		Removes the field named [field] from structure [o].
		
		This method is only guaranteed to work on anonymous structures.
		
		If [o] or [field] are null, the result is unspecified.
	**/
	public static function deleteField( o : Dynamic, field : String ) : Bool;

	/**
		Copies the fields of structure [o].
		
		This is only guaranteed to work on anonymous structures.
		
		If [o] is null, the result is unspecified.
	**/
	public static function copy<T>( o : T ) : T;

	/**
		Transform a function taking an array of arguments into a function that can
		be called with any number of arguments.
	**/
	@:overload(function( f : Array<Dynamic> -> Void ) : Dynamic {})
	public static function makeVarArgs( f : Array<Dynamic> -> Dynamic ) : Dynamic;

}
