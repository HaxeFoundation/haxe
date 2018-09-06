/*
 * Copyright (C)2005-2018 Haxe Foundation
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

// This file is generated from typedarray.webidl. Do not edit!

package js.html;

@:native("Int16Array")
extern class Int16Array implements ArrayBufferView implements ArrayAccess<Int>
{
	static inline var BYTES_PER_ELEMENT : Int = 2;
	
	@:pure
	static function of( items : haxe.extern.Rest<Array<Any>> ) : Int16Array;
	@:pure
	static function from( source : Array<Int>, ?mapFn : Int -> Int -> Int, ?thisArg : Any ) : Int16Array;
	@:native("BYTES_PER_ELEMENT")
	var BYTES_PER_ELEMENT_(default,null) : Int;
	var length(default,null) : Int;
	var buffer(default,null) : ArrayBuffer;
	var byteOffset(default,null) : Int;
	var byteLength(default,null) : Int;
	
	/** @throws DOMError */
	@:overload( function( length : Int ) : Void {} )
	@:overload( function( array : Int16Array ) : Void {} )
	@:overload( function( array : Array<Int> ) : Void {} )
	function new( buffer : ArrayBuffer, ?byteOffset : Int, ?length : Int ) : Void;
	@:overload( function( array : Int16Array, ?offset : Int ) : Void {} )
	function set( array : Array<Int>, ?offset : Int ) : Void;
	function copyWithin( target : Int, start : Int, ?end : Int ) : Int16Array;
	function every( callback : Int -> Int -> Int16Array -> Bool, ?thisArg : Any ) : Bool;
	function fill( value : Int, ?start : Int, ?end : Int ) : Int16Array;
	function filter( callbackfn : Int -> Int -> Int16Array -> Any, ?thisArg : Any ) : Int16Array;
	function find( predicate : Int -> Int -> Int16Array -> Bool, ?thisArg : Any ) : Any;
	function findIndex( predicate : Int -> Int -> Int16Array -> Bool, ?thisArg : Any ) : Int;
	function forEach( callbackfn : Int -> Int -> Int16Array -> Void, ?thisArg : Any ) : Void;
	function indexOf( searchElement : Int, ?fromIndex : Int ) : Int;
	function join( ?separator : String ) : String;
	function lastIndexOf( searchElement : Int, ?fromIndex : Int ) : Int;
	function map( callbackfn : Int -> Int -> Int16Array -> Int, ?thisArg : Any ) : Int16Array;
	@:overload( function( callbackfn : Int -> Int -> Int -> Int16Array -> Int ) : Int {} )
	function reduce( callbackfn : Any -> Int -> Int -> Int16Array -> Any, initialValue : Any ) : Any;
	@:overload( function( callbackfn : Int -> Int -> Int -> Int16Array -> Int ) : Int {} )
	function reduceRight( callbackfn : Any -> Int -> Int -> Int16Array -> Any, initialValue : Any ) : Any;
	function reverse() : Int16Array;
	function slice( ?start : Int, ?end : Int ) : Int16Array;
	function some( callbackfn : Int -> Int -> Int16Array -> Bool, ?thisArg : Any ) : Bool;
	function sort( ?compareFn : Int -> Int -> Int ) : Int16Array;
	function subarray( begin : Int, ?end : Int ) : Int16Array;
}