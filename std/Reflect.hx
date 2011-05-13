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
extern class Reflect {

	/**
		Tells if an object has a field set. This doesn't take into account the object prototype (class methods).
	**/
	public static function hasField( o : Dynamic, field : String ) : Bool;

	/**
		Returns the field of an object, or null if [o] is not an object or doesn't have this field.
	**/
	public static function field( o : Dynamic, field : String ) : Dynamic;


	/**
		Set an object field value.
	**/
	public static function setField( o : Dynamic, field : String, value : Dynamic ) : Void;

	/**
		Call a method with the given object and arguments.
	**/
	public static function callMethod( o : Dynamic, func : Dynamic, args : Array<Dynamic> ) : Dynamic;

	/**
		Returns the list of fields of an object, excluding its prototype (class methods).
	**/
	public static function fields( o : Dynamic ) : Array<String>;

	/**
		Tells if a value is a function or not.
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
		Delete an object field.
	**/
	public static function deleteField( o : Dynamic, f : String ) : Bool;

	/**
		Make a copy of the fields of an object.
	**/
	public static function copy<T>( o : T ) : T;

	/**
		Transform a function taking an array of arguments into a function that can
		be called with any number of arguments.
	**/
	public static function makeVarArgs( f : Array<Dynamic> -> Dynamic ) : Dynamic;

}
