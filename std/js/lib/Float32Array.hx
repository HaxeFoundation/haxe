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

// This file is generated from typedarray.webidl. Do not edit!

package js.lib;

@:native("Float32Array")
extern class Float32Array implements ArrayBufferView implements ArrayAccess<Float> {
	static inline var BYTES_PER_ELEMENT : Int = 4;
	
	@:pure
	static function of( items : haxe.extern.Rest<Array<Dynamic>> ) : Float32Array;
	@:pure
	static function from( source : Array<Float>, ?mapFn : Float -> Float -> Float, ?thisArg : Dynamic ) : Float32Array;
	@:native("BYTES_PER_ELEMENT")
	var BYTES_PER_ELEMENT_(default,null) : Int;
	var length(default,null) : Int;
	var buffer(default,null) : ArrayBuffer;
	var byteOffset(default,null) : Int;
	var byteLength(default,null) : Int;
	
	/** @throws DOMError */
	@:overload( function( length : Int ) : Void {} )
	@:overload( function( array : Float32Array ) : Void {} )
	@:overload( function( array : Array<Float> ) : Void {} )
	function new( buffer : ArrayBuffer, ?byteOffset : Int, ?length : Int ) : Void;
	@:overload( function( array : Float32Array, ?offset : Int ) : Void {} )
	function set( array : Array<Float>, ?offset : Int ) : Void;
	function copyWithin( target : Int, start : Int, ?end : Int ) : Float32Array;
	function every( callback : Float -> Int -> Float32Array -> Bool, ?thisArg : Dynamic ) : Bool;
	function fill( value : Float, ?start : Int, ?end : Int ) : Float32Array;
	function filter( callbackfn : Float -> Int -> Float32Array -> Dynamic, ?thisArg : Dynamic ) : Float32Array;
	function find( predicate : Float -> Int -> Float32Array -> Bool, ?thisArg : Dynamic ) : Dynamic;
	function findIndex( predicate : Float -> Int -> Float32Array -> Bool, ?thisArg : Dynamic ) : Int;
	function forEach( callbackfn : Float -> Int -> Float32Array -> Void, ?thisArg : Dynamic ) : Void;
	function indexOf( searchElement : Float, ?fromIndex : Int ) : Int;
	function join( ?separator : String ) : String;
	function lastIndexOf( searchElement : Float, ?fromIndex : Int ) : Int;
	function map( callbackfn : Float -> Int -> Float32Array -> Float, ?thisArg : Dynamic ) : Float32Array;
	@:overload( function( callbackfn : Float -> Float -> Int -> Float32Array -> Float ) : Int {} )
	function reduce( callbackfn : Dynamic -> Float -> Int -> Float32Array -> Dynamic, initialValue : Dynamic ) : Dynamic;
	@:overload( function( callbackfn : Float -> Float -> Int -> Float32Array -> Float ) : Int {} )
	function reduceRight( callbackfn : Dynamic -> Float -> Int -> Float32Array -> Dynamic, initialValue : Dynamic ) : Dynamic;
	function reverse() : Float32Array;
	function slice( ?start : Int, ?end : Int ) : Float32Array;
	function some( callbackfn : Float -> Int -> Float32Array -> Bool, ?thisArg : Dynamic ) : Bool;
	function sort( ?compareFn : Float -> Float -> Int ) : Float32Array;
	function subarray( begin : Int, ?end : Int ) : Float32Array;
}