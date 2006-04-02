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

extern class Array<T> {

	var length : Int;

	function new() : Void;

	function concat( a : Array<T> ) : Array<T>;
	function join( sep : String ) : String;

	/**
		Removes the last element of the array and returns it.
	**/
	function pop() : T;

	/**
		Adds the element [x] at the end of the array.
	**/
	function push(x : T) : Int;

	function reverse() : Array<T>;

	/**
		Removes the first element and returns it.
	**/
	function shift() : T;

	/**
		Copies the range of the array starting at [pos] up to,
		but not including, [end]. Both [pos] and [end] can be
		negative to count from the end: -1 is the last item in
		the array.
	**/
	function slice( pos : Int, end : Int ) : Array<T>; // sub
	function sort( f : T -> T -> Int ) : Void;

	/**
		Removes [len] elements starting from [pos].
	**/
	function splice( pos : Int, len : Int ) : Array<T>; // removed elts
	// no toSource (js specific)
	function toString() : String;

	/**
		Adds the element [x] at the start of the array.
	**/
	function unshift( x : T ) : Void;
	// no valueOf (js specific)

	/* added */
	function insert( pos : Int, x : T ) : Void;

	/**
		Removes the first occurence of [x].
		Returns false if [x] was not present.
	**/
	function remove( x : T ) : Bool;
	function copy() : Array<T>;
	function iterator() : Iterator<T>;
	function indexes() : Iterator<Int>;

}
