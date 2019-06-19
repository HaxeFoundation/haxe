/*
 * Copyright (C)2005-2019 Haxe Foundation
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
package js.lib;

/**
	The `Uint8ClampedArray` typed array represents an array of 8-bit unsigned integers clamped
	to 0-255; if you specified a value that is out of the range of [0,255], 0 or 255 will be set instead;
	if you specify a non-integer, the nearest integer will be set. The contents are initialized to `0`.
	Once established, you can reference elements in the array using the object's methods, or using
	standard array index syntax (that is, using bracket notation).

	Documentation [Uint8ClampedArray](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Uint8ClampedArray) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Uint8ClampedArray$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).
**/
@:native("Uint8ClampedArray")
extern class Uint8ClampedArray implements ArrayAccess<Int> {
	static inline var BYTES_PER_ELEMENT : Int = 1;
	
	@:pure
	static function of( items : haxe.extern.Rest<Array<Dynamic>> ) : Uint8ClampedArray;
	@:pure
	static function from( source : Array<Int>, ?mapFn : Int -> Int -> Int, ?thisArg : Dynamic ) : Uint8ClampedArray;

	@:native("BYTES_PER_ELEMENT")
	final BYTES_PER_ELEMENT_ : Int;
	final length : Int;
	final buffer : ArrayBuffer;
	final byteOffset : Int;
	final byteLength : Int;
	
	/** @throws DOMError */
	@:overload( function( length : Int ) : Void {} )
	@:overload( function( array : Uint8ClampedArray ) : Void {} )
	@:overload( function( array : Array<Int> ) : Void {} )
	function new( buffer : ArrayBuffer, ?byteOffset : Int, ?length : Int ) : Void;
	@:overload( function( array : Uint8ClampedArray, ?offset : Int ) : Void {} )
	function set( array : Array<Int>, ?offset : Int ) : Void;
	function copyWithin( target : Int, start : Int, ?end : Int ) : Uint8ClampedArray;
	function every( callback : Int -> Int -> Uint8ClampedArray -> Bool, ?thisArg : Dynamic ) : Bool;
	function fill( value : Int, ?start : Int, ?end : Int ) : Uint8ClampedArray;
	function filter( callbackfn : Int -> Int -> Uint8ClampedArray -> Dynamic, ?thisArg : Dynamic ) : Uint8ClampedArray;
	function find( predicate : Int -> Int -> Uint8ClampedArray -> Bool, ?thisArg : Dynamic ) : Dynamic;
	function findIndex( predicate : Int -> Int -> Uint8ClampedArray -> Bool, ?thisArg : Dynamic ) : Int;
	function forEach( callbackfn : Int -> Int -> Uint8ClampedArray -> Void, ?thisArg : Dynamic ) : Void;
	function indexOf( searchElement : Int, ?fromIndex : Int ) : Int;
	function join( ?separator : String ) : String;
	function lastIndexOf( searchElement : Int, ?fromIndex : Int ) : Int;
	function map( callbackfn : Int -> Int -> Uint8ClampedArray -> Int, ?thisArg : Dynamic ) : Uint8ClampedArray;
	@:overload( function( callbackfn : Int -> Int -> Int -> Uint8ClampedArray -> Int ) : Int {} )
	function reduce( callbackfn : Dynamic -> Int -> Int -> Uint8ClampedArray -> Dynamic, initialValue : Dynamic ) : Dynamic;
	@:overload( function( callbackfn : Int -> Int -> Int -> Uint8ClampedArray -> Int ) : Int {} )
	function reduceRight( callbackfn : Dynamic -> Int -> Int -> Uint8ClampedArray -> Dynamic, initialValue : Dynamic ) : Dynamic;
	function reverse() : Uint8ClampedArray;
	function slice( ?start : Int, ?end : Int ) : Uint8ClampedArray;
	function some( callbackfn : Int -> Int -> Uint8ClampedArray -> Bool, ?thisArg : Dynamic ) : Bool;
	function sort( ?compareFn : Int -> Int -> Int ) : Uint8ClampedArray;
	function subarray( begin : Int, ?end : Int ) : Uint8ClampedArray;
}