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

private typedef VectorData<T> = #if flash10
	flash.Vector<T>
#elseif neko
	neko.NativeArray<T>
#else
	Array<T>
#end

/**
	A Vector is a storage of fixed size. It can be faster than Array on some
	targets, and is never slower.
**/
@:arrayAccess
abstract Vector(VectorData<T>)<T> {
	/**
		Creates a new Vector of length [length].

		Initially [this] Vector contains [length] neutral elements:
			- always null on dynamic targets
			- 0, 0.0 or false for Int, Float and Bool respectively on static
			targets
			- null for other types on static targets

		If [length] is less than or equal to 0, the result is unspecified.
	**/
	public inline function new(length:Int) {
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

		If [index] is negative or exceeds [this].length, the result is
		unspecified.
	**/
	public inline function get(index:Int):Null<T> {
		return this[index];
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

	/**
		Extracts the data of [this] Vector.

		This returns the internal representation type.
	**/
	public inline function toData():VectorData<T>
		return cast this

	/**
		Initializes a new Vector from [data].

		Since [data] is the internal representation of Vector, this is a no-op.

		If [data] is null, the corresponding Vector is also [null].
	**/
	static public inline function fromData<T>(data:VectorData<T>):Vector<T>
		return cast data

	/**
		Creates a new Vector by copying the elements of [array].

		This always creates a copy, even on platforms where the internal
		representation is Array.

		The elements are not copied and retain their identity, so
		a[i] == Vector.fromArrayCopy(a).get(i) is true for any valid i.

		If [array] is null, the result is unspecified.
	**/
	static public inline function fromArrayCopy<T>(array:Array<T>):Vector<T> {
		// TODO: Optimize this for flash (and others?)
		var vec = new Vector<T>(array.length);
		for (i in 0...array.length)
			vec.set(i, array[i]);
		return vec;
	}
}
