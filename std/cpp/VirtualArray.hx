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

package cpp;

@:native("cpp::VirtualArray")
@:coreType extern class NativeVirtualArray implements ArrayAccess<Dynamic> {
	function new():Void;
	var length(get, null):Int;
	// concat<T>( a:Array<T> ) : Array<T> ?
	function concat(a:VirtualArray):VirtualArray;
	function join(sep:String):String;
	function pop():Dynamic;
	function push(x:Dynamic):Int;
	function reverse():Void;
	function shift():Dynamic;
	function slice(pos:Int, ?end:Int):VirtualArray;
	function sort(f:Dynamic->Dynamic->Int):Void;
	function splice(pos:Int, len:Int):VirtualArray;
	function toString():String;
	function unshift(x:Dynamic):Void;
	function insert(pos:Int, x:Dynamic):Void;
	function remove(x:Dynamic):Bool;
	function indexOf(x:Dynamic, ?fromIndex:Int):Int;
	function lastIndexOf(x:Dynamic, ?fromIndex:Int):Int;
	function copy():VirtualArray;
	function iterator():Iterator<Dynamic>;
	function keyValueIterator():KeyValueIterator<Int, Dynamic>;
	function map<S>(f:Dynamic->S):VirtualArray;
	function filter(f:Dynamic->Bool):VirtualArray;
	function resize(len:Int):Void;
}

abstract VirtualArray(NativeVirtualArray) {
	// Add these two functions...
	@:from extern inline static public function fromArray<T>(a:Array<T>):VirtualArray
		return untyped a;

	@:to extern inline public function toArray<T>():Array<T>
		return untyped this;

	// The rest is just boiler-plate
	inline public function new()
		this = new NativeVirtualArray();

	@:arrayAccess extern inline function get(idx:Int):Dynamic
		return untyped this[idx];

	@:arrayAccess extern inline function set<T>(pos:Int, value:T):T
		return untyped this[idx] = value;

	public var length(get, never):Int;

	extern inline public function get_length():Int
		return this.length;

	// concat<T>( a:Array<T> ) : Array<T> ?
	extern inline public function concat(a:VirtualArray):VirtualArray
		return this.concat(a);

	extern inline public function join(sep:String):String
		return this.join(sep);

	extern inline public function pop():Dynamic
		return this.pop();

	extern inline public function push(x:Dynamic):Int
		return this.push(x);

	extern inline public function reverse():Void
		this.reverse();

	extern inline public function shift():Dynamic
		return this.shift();

	extern inline public function slice(pos:Int, ?end:Int):VirtualArray
		return this.slice(pos, end);

	extern inline public function sort(f:Dynamic->Dynamic->Int):Void
		this.sort(f);

	extern inline public function splice(pos:Int, len:Int):VirtualArray
		return this.slice(pos, len);

	extern inline public function unshift(x:Dynamic):Void
		this.unshift(x);

	extern inline public function insert(pos:Int, x:Dynamic):Void
		this.insert(pos, x);

	extern inline public function remove(x:Dynamic):Bool
		return this.remove(x);

	extern inline public function indexOf(x:Dynamic, ?fromIndex:Int):Int
		return this.indexOf(x, fromIndex);

	extern inline public function lastIndexOf(x:Dynamic, ?fromIndex:Int):Int
		return this.lastIndexOf(x, fromIndex);

	extern inline public function copy():VirtualArray
		return this.copy();

	extern inline public function iterator():Iterator<Dynamic>
		return this.iterator();

	extern inline public function keyValueIterator():KeyValueIterator<Int, Dynamic>
		return this.keyValueIterator();

	extern inline public function map<S>(f:Dynamic->S):VirtualArray
		return this.map(f);

	extern inline public function filter(f:Dynamic->Bool):VirtualArray
		return this.filter(f);

	extern inline public function resize(len:Int):Void
		return this.resize(len);
}
