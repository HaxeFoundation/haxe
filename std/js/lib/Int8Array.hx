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

import js.lib.intl.NumberFormat.NumberFormatOptions;

/**
	The `Int8Array` typed array represents an array of twos-complement 8-bit signed integers. The
	contents are initialized to 0. Once established, you can reference elements in the array using
	the object's methods, or using standard array index syntax (that is, using bracket notation).

	Documentation [Int8Array](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Int8Array) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Int8Array$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).
**/
@:native("Int8Array")
extern class Int8Array implements ArrayBufferView implements ArrayAccess<Int> {
	/**
		Returns a number value of the element size. 1 in the case of an `Int8Array`.
	 */
	static final BYTES_PER_ELEMENT:Int;

	/**
		Creates a new `Int8Array` from an array-like or iterable object. See also [Array.from()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/from).
	 */
	@:overload(function(source:{}, ?mapFn:(value:Int) -> Int, ?thisArg:Any):Int8Array {})
	@:pure static function from(source:{}, ?mapFn:(value:Int, index:Int) -> Int, ?thisArg:Any):Int8Array;

	/**
		Creates a new `Int8Array` with a variable number of arguments. See also [Array.of()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/of).
	 */
	@:pure static function of(elements:haxe.extern.Rest<Dynamic>):Int8Array;

	/**
		Returns a number value of the element size.
	 */
	@:native("BYTES_PER_ELEMENT")
	final BYTES_PER_ELEMENT_:Int;

	/**
		Returns the `ArrayBuffer` referenced by the `Int8Array` Fixed at construction time and thus read only.
	 */
	final buffer:ArrayBuffer;

	/**
		Returns the length (in bytes) of the `Int8Array` from the start of its `ArrayBuffer`. Fixed at construction time and thus read only.
	 */
	final byteLength:Int;

	/**
		Returns the offset (in bytes) of the `Int8Array` from the start of its `ArrayBuffer`. Fixed at construction time and thus read only.
	 */
	final byteOffset:Int;

	/**
		Returns the number of elements hold in the `Int8Array`. Fixed at construction time and thus read only.
	 */
	final length:Int;

	/** @throws DOMError */
	@:overload(function(length:Int):Void {})
	@:overload(function(object:{}):Void {})
	@:pure function new(buffer:ArrayBuffer, ?byteOffset:Int, ?length:Int):Void;

	/**
		Copies a sequence of array elements within the array.
		See also [Array.prototype.copyWithin()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/copyWithin).
	 */
	function copyWithin(target:Int, start:Int, ?end:Int):Int8Array;

	/**
		Returns a new Array Iterator object that contains the key/value pairs for each index in the array.
		See also [Array.prototype.entries()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/entries).
	 */
	@:pure function entries():js.lib.Iterator<KeyValue<Int, Int>>;

	/**
		Tests whether all elements in the array pass the test provided by a function.
		See also [Array.prototype.every()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/every).
	 */
	@:overload(function(callback:(currentValue:Int) -> Bool, ?thisArg:Any):Bool {})
	@:overload(function(callback:(currentValue:Int, index:Int) -> Bool, ?thisArg:Any):Bool {})
	function every(callback:(currentValue:Int, index:Int, array:Int8Array) -> Bool, ?thisArg:Any):Bool;

	/**
		Fills all the elements of an array from a start index to an end index with a static value.
		See also [Array.prototype.fill()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/fill).
	 */
	function fill(value:Int, ?start:Int, ?end:Int):Int8Array;

	/**
		Creates a new array with all of the elements of this array for which the provided filtering function returns true.
		See also [Array.prototype.filter()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/filter).
	 */
	@:overload(function(callback:(element:Int) -> Bool, ?thisArg:Any):Int8Array {})
	@:overload(function(callback:(element:Int, index:Int) -> Bool, ?thisArg:Any):Int8Array {})
	function filter(callback:(element:Int, index:Int, array:Int8Array) -> Bool, ?thisArg:Any):Int8Array;

	/**
		Returns the found value in the array, if an element in the array satisfies the provided testing function or undefined if not found.
		See also [Array.prototype.find()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/find).
	 */
	@:overload(function(callback:(element:Int) -> Bool, ?thisArg:Any):Null<Int> {})
	@:overload(function(callback:(element:Int, index:Int) -> Bool, ?thisArg:Any):Null<Int> {})
	function find(callback:(element:Int, index:Int, array:Int8Array) -> Bool, ?thisArg:Any):Null<Int>;

	/**
		Returns the found index in the array, if an element in the array satisfies the provided testing function or -1 if not found.
		See also [Array.prototype.findIndex()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/findIndex).
	 */
	@:overload(function(callback:(element:Int) -> Bool, ?thisArg:Any):Int {})
	@:overload(function(callback:(element:Int, index:Int) -> Bool, ?thisArg:Any):Int {})
	function findIndex(callback:(element:Int, index:Int, array:Int8Array) -> Bool, ?thisArg:Any):Int;

	/**
		Calls a function for each element in the array.
		See also [Array.prototype.forEach()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/forEach).
	 */
	@:overload(function(callback:(element:Int) -> Void, ?thisArg:Any):Void {})
	@:overload(function(callback:(element:Int, index:Int) -> Void, ?thisArg:Any):Void {})
	function forEach(callback:(element:Int, index:Int, array:Int8Array) -> Void, ?thisArg:Any):Void;

	/**
		Determines whether a typed array includes a certain element, returning true or false as appropriate.
		See also [Array.prototype.includes()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/includes).
	 */
	@:pure function includes(searchElement:Int, ?fromIndex:Int):Bool;

	/**
		Returns the first (least) index of an element within the array equal to the specified value, or -1 if none is found.
		See also [Array.prototype.indexOf()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/indexOf).
	 */
	@:pure function indexOf(searchElement:Int, ?fromIndex:Int):Int;

	/**
		Joins all elements of an array into a string.
		See also [Array.prototype.join()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/join).
	 */
	@:pure function join(?separator:String):String;

	/**
		Returns a new Array Iterator that contains the keys for each index in the array.
		See also [Array.prototype.keys()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/keys).
	 */
	@:pure function keys():js.lib.Iterator<Int>;

	/**
		Returns the last (greatest) index of an element within the array equal to the specified value, or -1 if none is found.
		See also [Array.prototype.lastIndexOf()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/lastIndexOf).
	 */
	@:pure function lastIndexOf(searchElement:Int, ?fromIndex:Int):Int;

	/**
		Creates a new array with the results of calling a provided function on every element in this array.
		See also [Array.prototype.map()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/map).
	 */
	@:overload(function(callback:(element:Int) -> Int, ?thisArg:Any):Int8Array {})
	@:overload(function(callback:(element:Int, index:Int) -> Int, ?thisArg:Any):Int8Array {})
	function map(callback:(element:Int, index:Int, array:Int8Array) -> Int, ?thisArg:Any):Int8Array;

	/**
		Apply a function against an accumulator and each value of the array (from left-to-right) as to reduce it to a single value.
		See also [Array.prototype.reduce()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/reduce).
	 */
	@:overload(function<T>(callback:(previousValue:T, currentValue:Int) -> T, initialValue:T):T {})
	@:overload(function<T>(callback:(previousValue:T, currentValue:Int, index:Int) -> T, initialValue:T):T {})
	@:overload(function(callbackfn:(previousValue:Int, currentValue:Int) -> Int):Int {})
	@:overload(function(callbackfn:(previousValue:Int, currentValue:Int, index:Int) -> Int):Int {})
	@:overload(function(callbackfn:(previousValue:Int, currentValue:Int, index:Int, array:Int8Array) -> Int):Int {})
	function reduce<T>(callback:(previousValue:T, currentValue:Int, index:Int, array:Int8Array) -> T, initialValue:T):T;

	/**
		Apply a function against an accumulator and each value of the array (from right-to-left) as to reduce it to a single value.
		See also [Array.prototype.reduceRight()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/reduceRight).
	 */
	@:overload(function<T>(callback:(previousValue:T, currentValue:Int) -> T, initialValue:T):T {})
	@:overload(function<T>(callback:(previousValue:T, currentValue:Int, index:Int) -> T, initialValue:T):T {})
	@:overload(function(callbackfn:(previousValue:Int, currentValue:Int) -> Int):Int {})
	@:overload(function(callbackfn:(previousValue:Int, currentValue:Int, index:Int) -> Int):Int {})
	@:overload(function(callbackfn:(previousValue:Int, currentValue:Int, index:Int, array:Int8Array) -> Int):Int {})
	function reduceRight<T>(callback:(previousValue:T, currentValue:Int, index:Int, array:Int8Array) -> T, initialValue:T):T;

	/**
		Reverses the order of the elements of an array — the first becomes the last, and the last becomes the first.
		See also [Array.prototype.reverse()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/reverse).
	 */
	function reverse():Int8Array;

	/**
		Stores multiple values in the typed array, reading input values from a specified array.
	 */
	@:overload(function(array:Int8Array, ?offset:Int):Void {})
	@:overload(function(array:Uint8Array, ?offset:Int):Void {})
	@:overload(function(array:Uint8ClampedArray, ?offset:Int):Void {})
	@:overload(function(array:Int16Array, ?offset:Int):Void {})
	@:overload(function(array:Uint16Array, ?offset:Int):Void {})
	@:overload(function(array:Int32Array, ?offset:Int):Void {})
	@:overload(function(array:Uint32Array, ?offset:Int):Void {})
	@:overload(function(array:Float32Array, ?offset:Int):Void {})
	@:overload(function(array:Float64Array, ?offset:Int):Void {})
	function set(array:Array<Int>, ?offset:Int):Void;

	/**
		Extracts a section of an array and returns a new array.
		See also [Array.prototype.slice()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/slice).
	 */
	@:pure function slice(?start:Int, ?end:Int):Int8Array;

	/**
		Returns true if at least one element in this array satisfies the provided testing function.
		See also [Array.prototype.some()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/some).
	 */
	@:overload(function(callback:(element:Int) -> Bool, ?thisArg:Any):Bool {})
	@:overload(function(callback:(element:Int, index:Int) -> Bool, ?thisArg:Any):Bool {})
	function some(callback:(element:Int, index:Int, array:Int8Array) -> Bool, ?thisArg:Any):Bool;

	/**
		Sorts the elements of an array in place and returns the array.
		See also [Array.prototype.sort()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/sort).
	 */
	function sort(?compareFn:(x:Int, y:Int) -> Int):Int8Array;

	/**
		Returns a new TypedArray from the given start and end element index.
	 */
	@:pure function subarray(?begin:Int, ?end:Int):Int8Array;

	/**
		Returns a new Array Iterator object that contains the values for each index in the array.
		See also [Array.prototype.values()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/values).
	 */
	@:pure function values():js.lib.Iterator<Int>;

	/**
		Returns a string representing the array and its elements.
		See also [Array.prototype.toString()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/toString).
	 */
	@:pure function toLocaleString(?locales:String, ?options:NumberFormatOptions):String;

	/**
		Returns a string representing the array and its elements.
		See also [Array.prototype.toString()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/toString).
	 */
	@:pure function toString():String;
}
