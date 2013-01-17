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
		The length of [this] Array.
	**/
	var length(default,null) : Int;

	/**
		Creates a new Array.
	**/
	function new() : Void;

	/**
		Returns a new Array by appending the elements of [a] to the elements of
		[this] Array.
		
		This operation does not modify [this] Array.
		
		If [a] is the empty Array [], a copy of [this] Array is returned.
		
		The length of the returned Array is equal to the sum of [this].length
		and [a].length.
		
		If [a] is null, the result is unspecified.
	**/
	function concat( a : Array<T> ) : Array<T>;

	/**
		Returns a string representation of [this] Array, with [sep] separating
		each element.
		
		The result of this operation is equal to Std.string(this[0]) + sep +
		Std.string(this[1]) + sep + ... + sep + Std.string(this[this.length-1]).
		
		If [this] is the empty Array [], the result is the empty String "". If
		[this] has exactly one element, the result is equal to a call to
		Std.string(this[0]).
		
		If [a] is null, the result is unspecified.
	**/
	function join( sep : String ) : String;

	/**
		Removes the last element of [this] Array and returns it.
		
		This operation modifies [this] Array in place.
		
		If [this] has at least one element, [this].length will decrease by 1.
		
		If [this] is the empty Array [], null is returned and the length remains
		0.
	**/
	function pop() : Null<T>;

	/**
		Adds the element [x] at the end of [this] Array and returns the offset
		it was added at.
		
		This operation modifies [this] Array in place.
		
		[this].length will increase by 1.
	**/
	function push(x : T) : Int;

	/**
		Reverse the order of elements of [this] Array.
		
		This operation modifies [this] Array in place.
		
		If [this].length < 2, [this] remains unchanged.
	**/
	function reverse() : Void;

	/**
		Removes the first element of [this] Array and returns it.
		
		This operation modifies [this] Array in place.
		
		If [this] has at least one element, [this].length and the index of each
		remaining element is decreased by 1.
		
		If [this] is the empty Array [], null is returned and the length remains
		0.
	**/
	function shift() : Null<T>;

	/**
		Creates a shallow copy of the range of [this] Array, starting at and
		including [pos], up to but not including [end].
		
		This operation does not modify [this] Array.
		
		The elements are not copied and retain their identity.
		
		If [end] is omitted or exceeds [this].length, it defaults to the end of
		[this] Array.
		
		If [pos] or [end] are negative, their offsets are calculated from the
		end	of [this] Array by [this].length + [pos] and [this].length + [end]
		respectively. If this yields a negative value, 0 is used instead.
		
		If [pos] exceeds [this].length or if [end} exceeds or equals [pos],
		the result is [].
	**/
	function slice( pos : Int, ?end : Int ) : Array<T>;

	/**
		Sorts [this] Array according to the comparison function [f], where
		[f(x,y)] returns 0 if x == y, a positive Int if x > y and a
		negative Int if x < y.
		
		This operation modifies [this] Array in place.
		
		The sort operation is robust: Equal elements will retain their order.
		
		If [f] is null, the result is unspecified.
	**/
	function sort( f : T -> T -> Int ) : Void;

	/**
		Removes [len] elements from [this] Array, starting at and including
		[pos], an returns them.
		
		This operation modifies [this] Array in place.
		
		If [len] is < 0 or [pos] exceeds [this].length, the result is the empty
		Array [].
		
		If [pos] is negative, its value is calculated from the end	of [this]
		Array by [this].length + [pos]. If this yields a negative value, 0 is
		used instead.
		
		If the sum of the resulting values for [len] and [pos] exceed
		[this].length, this operation will affect the elements from [pos] to the
		end of [this] Array.
		
		The length of the returned Array is equal to the new length of [this]
		Array subtracted from the original length of [this] Array. In other
		words, each element of the original [this] Array either remains in
		[this] Array or becomes an element of the returned Array.
	**/
	function splice( pos : Int, len : Int ) : Array<T>;

	/**
		Returns a string representation of [this] Array.
		
		The result will include the individual elements' String representations
		separated by comma. The enclosing [ ] may be missing on some platforms,
		use Std.string() to get a String representation that is consistent
		across platforms.
	**/
	function toString() : String;

	/**
		Adds the element [x] at the start of [this] Array.
		
		This operation modifies [this] Array in place.
		
		[this].length and the index of each Array element increases by 1.
	**/
	function unshift( x : T ) : Void;

	/**
		Inserts the element [x] at the position [pos].
		
		This operation modifies [this] Array in place.
		
		The offset is calculated like so:
			
		- If [pos] exceeds [this].length, the offset is [this].length.
		- If [pos] is negative, the offset is calculated from the end of [this]
		Array, i.e. [this].length + [pos]. If this yields a negative value,
		the offset is 0.
		- Otherwise, the offset is [pos].
		
		If the resulting offset does not exceed [this].length, all elements from
		and including that offset to the end of [this] Array are moved one index
		ahead.
	**/
	function insert( pos : Int, x : T ) : Void;

	/**
		Removes the first occurence of [x] in [this] Array.
		
		This operation modifies [this] Array in place.
		
		If [x] is found by checking standard equality, it is removed from [this]
		Array and all following elements are reindexed acoordingly. The function
		then returns true.
		
		If [x] is not found, [this] Array is not changed and the function
		returns false.
	**/
	function remove( x : T ) : Bool;

	/**
		Returns a shallow copy of [this] Array.
		
		The elements are not copied and retain their identity, so
		a[i] == a.copy()[i] is true for any valid i. However, a == a.copy() is
		always false.
	**/
	function copy() : Array<T>;

	/**
		Returns an iterator of the Array values.
	**/
	function iterator() : Iterator<T>;

	function map<S>( f : T -> S ) : Array<S>;
	function filter( f : T -> Bool ) : Array<T>;
}
