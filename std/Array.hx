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
	An Array is a storage for values. You can access it using indexes or
	with its API. On the server side, it's often better to use a [List] which
	is less memory and CPU consuming, unless you really need indexed access.
**/
extern class Array<T> {

	/**
		The length of the Array
	**/
	var length(default,null) : Int;

	/**
		Creates a new Array.
	**/
	function new() : Void;

	/**
		Returns a new Array by appending [a] to [this].
	**/
	function concat( a : Array<T> ) : Array<T>;

	/**
		Returns a representation of an array with [sep] for separating each element.
	**/
	function join( sep : String ) : String;

	/**
		Removes the last element of the array and returns it.
	**/
	function pop() : Null<T>;

	/**
		Adds the element [x] at the end of the array.
	**/
	function push(x : T) : Int;

	/**
		Reverse the order of elements of the Array.
	**/
	function reverse() : Void;

	/**
		Removes the first element and returns it.
	**/
	function shift() : Null<T>;

	/**
		Copies the range of the array starting at [pos] up to,
		but not including, [end]. Both [pos] and [end] can be
		negative to count from the end: -1 is the last item in
		the array.
	**/
	function slice( pos : Int, ?end : Int ) : Array<T>;

	/**
		Sort the Array according to the comparison function [f].
		[f(x,y)] should return [0] if [x == y], [>0] if [x > y]
		and [<0] if [x < y].
	**/
	function sort( f : T -> T -> Int ) : Void;

	/**
		Removes [len] elements starting from [pos] an returns them.
	**/
	function splice( pos : Int, len : Int ) : Array<T>;

	/**
		Returns a displayable representation of the Array content.
	**/
	function toString() : String;

	/**
		Adds the element [x] at the start of the array.
	**/
	function unshift( x : T ) : Void;

	/**
		Inserts the element [x] at the position [pos].
		All elements after [pos] are moved one index ahead.
	**/
	function insert( pos : Int, x : T ) : Void;

	/**
		Removes the first occurence of [x].
		Returns false if [x] was not present.
		Elements are compared by using standard equality.
	**/
	function remove( x : T ) : Bool;

	/**
		Returns a copy of the Array. The values are not
		copied, only the Array structure.
	**/
	function copy() : Array<T>;

	/**
		Returns an iterator of the Array values.
	**/
	function iterator() : Iterator<T>;

}
