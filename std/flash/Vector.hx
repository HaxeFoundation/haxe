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
package flash;

/**
	The Vector class is very similar to Array but is only supported by the Flash Player 10+
**/
@:require(flash10) extern class Vector<T> implements ArrayAccess<T> {

	var length : Int;
	var fixed : Bool;

	function new( ?length : UInt, ?fixed : Bool ) : Void;
	function concat( ?a : Vector<T> ) : Vector<T>;
	function join( sep : String ) : String;
	function pop() : Null<T>;
	function push(x : T) : Int;
	function reverse() : Void;
	function shift() : Null<T>;
	function unshift( x : T ) : Void;
	function slice( ?pos : Int, ?end : Int ) : Vector<T>;
	function sort( f : T -> T -> Int ) : Void;
	function splice( pos : Int, len : Int ) : Vector<T>;
	function toString() : String;
	function indexOf( x : T, ?from : Int ) : Int;
	function lastIndexOf( x : T, ?from : Int ) : Int;

	public inline static function ofArray<T>( v : Array<T> ) : Vector<T> {
		return untyped __vector__(v);
	}

	public inline static function convert<T,U>( v : Vector<T> ) : Vector<U> {
		return untyped __vector__(v);
	}

}
