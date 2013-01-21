/*
 * Copyright (C)2005-2013 Haxe Foundation
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
package haxe;

/**
	A Vector is a storage of fixed size. It can be faster than Array on some
	targets, and is never slower.
**/
	
typedef VImpl<T> = #if flash9
	flash.Vector<T>
#elseif neko
	neko.NativeArray<T>
#else
	Array<T>
#end

abstract Vector(VImpl<T>)<T> {
	/**
		Creates a new Vector of length [length].
		
		Initially [this] Vector contains [length] neutral elements:
			- always null on dynamic targets
			- 0, 0.0 or false for Int, Float and Bool respectively on static
			targets
			- null for other types on static targets
			
		If [length] is less than or equal to 0, an exception is thrown.
	**/
	public inline function new(length:Int) {
		if (length <= 0)
			throw "Invalid length, must be >= 1";
		#if flash9
			this = new flash.Vector<T>(length, true);
		#elseif neko
			this = untyped __dollar__amake(length);
		#else
			this = [];
			#if cpp
				untyped this.__SetSize(length);
			#else
				untyped this.length = length;
			#end
		#end
	}

	/**
		Returns the value at index [index].
		
		If [index] is negative or exceeds [this].length, null is returned.
	**/
    public inline function get(index:Int):Null<T> {
		#if (flash9 || cpp || js || java)
		return index < 0 || index >= this.length ? null : this[index];
		#else
		return this[index];
		#end
	}
	
	/**
		Sets the value at index [index] to [val].
		
		If [index] is negative or exceeds [this].length, the result is
		unspecified.
	**/
    public inline function set(index:Int, val:T):T {
		return this[index] = val;
	}
	
	/**
		Returns the length of [this] Vector.
	**/
    public inline function length():Int {
		#if neko
			return untyped __dollar__asize(this);
		#else
			return untyped this.length;
		#end
	}
	
	@:from static public inline function fromArray(arr:Array<T>) {
		// TODO: Optimize this for flash (and others?)
		var vec = new Vector<T>(arr.length);
		for (i in 0...arr.length)
			vec.set(i, arr[i]);
		return vec;
	}
}