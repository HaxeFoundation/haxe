/*
 * Copyright (C)2005-2017 Haxe Foundation
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
package haxe.ds;

#if cpp
using cpp.NativeArray;
#end

private typedef VectorData<T> = #if flash10
	flash.Vector<T>
#elseif neko
	neko.NativeArray<T>
#elseif cs
	cs.NativeArray<T>
#elseif java
	java.NativeArray<T>
#elseif lua
    lua.Table<Int,T>
#else
	Array<T>
#end

/**
	A Vector is a storage of fixed size. It can be faster than Array on some
	targets, and is never slower.

	@see https://haxe.org/manual/std-vector.html
**/
abstract Vector<T>(VectorData<T>) {
	/**
		Creates a new Vector of length `length`.

		Initially `this` Vector contains `length` neutral elements:

		- always null on dynamic targets
		- 0, 0.0 or false for Int, Float and Bool respectively on static targets
		- null for other types on static targets

		If `length` is less than or equal to 0, the result is unspecified.
	**/
	public inline function new(length:Int) {
		#if flash10
			this = new flash.Vector<T>(length, true);
		#elseif neko
			this = untyped __dollar__amake(length);
		#elseif js
			this = untyped __new__(Array, length);
		#elseif cs
			this = new cs.NativeArray(length);
		#elseif java
			this = new java.NativeArray(length);
		#elseif cpp
			this = NativeArray.create(length);
		#elseif python
			this = python.Syntax.pythonCode("[{0}]*{1}", null, length);
		#elseif lua
			this = untyped __lua_table__({length:length});
		#else
			this = [];
			untyped this.length = length;
		#end
	}

	/**
		Returns the value at index `index`.

		If `index` is negative or exceeds `this.length`, the result is
		unspecified.
	**/
	@:op([]) public inline function get(index:Int):T {
		#if cpp
		return this.unsafeGet(index);
		#elseif python
		return python.internal.ArrayImpl.unsafeGet(this, index);
		#else
		return this[index];
		#end
	}

	/**
		Sets the value at index `index` to `val`.

		If `index` is negative or exceeds `this.length`, the result is
		unspecified.
	**/
	@:op([]) public inline function set(index:Int, val:T):T {
		#if cpp
		return this.unsafeSet(index,val);
		#elseif python
		return python.internal.ArrayImpl.unsafeSet(this, index, val);
		#else
		return this[index] = val;
		#end
	}

	/**
		Returns the length of `this` Vector.
	**/
	public var length(get, never):Int;

	inline function get_length():Int {
		#if neko
			return untyped __dollar__asize(this);
		#elseif cs
			return this.Length;
		#elseif java
			return this.length;
		#elseif python
			return this.length;
		#else
			return untyped this.length;
		#end
	}

	/**
		Copies `length` of elements from `src` Vector, beginning at `srcPos` to
		`dest` Vector, beginning at `destPos`

		The results are unspecified if `length` results in out-of-bounds access,
		or if `src` or `dest` are null
	**/
	public static #if (cs || java || neko || cpp) inline #end function blit<T>(src:Vector<T>, srcPos:Int, dest:Vector<T>, destPos:Int, len:Int):Void
	{
		#if neko
			untyped __dollar__ablit(dest,destPos,src,srcPos,len);
		#elseif java
			java.lang.System.arraycopy(src, srcPos, dest, destPos, len);
		#elseif cs
			cs.system.Array.Copy(cast src, srcPos,cast dest, destPos, len);
		#elseif cpp
			dest.toData().blit(destPos,src.toData(), srcPos,len);
		#else
			if (src == dest) {
				if (srcPos < destPos) {
					var i = srcPos + len;
					var j = destPos + len;
					for (k in 0...len) {
						i--;
						j--;
						src[j] = src[i];
					}
				} else if (srcPos > destPos) {
					var i = srcPos;
					var j = destPos;
					for (k in 0...len) {
						src[j] = src[i];
						i++;
						j++;
					}
				}
			} else {
				for (i in 0...len) {
					dest[destPos + i] = src[srcPos + i];
				}
			}
		#end
	}

	/**
		Creates a new Array, copy the content from the Vector to it, and returns it.
	**/
	public #if (flash || cpp || js || java) inline #end function toArray():Array<T> {
		#if cpp
			return this.copy();
		#elseif python
			return this.copy();
		#elseif js
			return this.slice(0);
		#else
			var a = new Array();
			var len = length;
			#if (neko)
			// prealloc good size
			if( len > 0 ) a[len - 1] = get(0);
			#end
			for( i in 0...len )
				a[i] = get(i);
			return a;
		#end
	}

	/**
		Extracts the data of `this` Vector.

		This returns the internal representation type.
	**/
	public inline function toData():VectorData<T>
		return cast this;

	/**
		Initializes a new Vector from `data`.

		Since `data` is the internal representation of Vector, this is a no-op.

		If `data` is null, the corresponding Vector is also `null`.
	**/
	static public inline function fromData<T>(data:VectorData<T>):Vector<T>
		return cast data;

	/**
		Creates a new Vector by copying the elements of `array`.

		This always creates a copy, even on platforms where the internal
		representation is Array.

		The elements are not copied and retain their identity, so
		`a[i] == Vector.fromArrayCopy(a).get(i)` is true for any valid i.

		If `array` is null, the result is unspecified.
	**/
	#if as3 @:extern #end
	static public inline function fromArrayCopy<T>(array:Array<T>):Vector<T> {
		#if python
		return cast array.copy();
		#elseif flash10
		return fromData(flash.Vector.ofArray(array));
		#elseif java
		return fromData(java.Lib.nativeArray(array,false));
		#elseif cs
		return fromData(cs.Lib.nativeArray(array,false));
		#elseif cpp
		return cast array.copy();
		#elseif js
		return fromData(array.slice(0));
		#else
		// TODO: Optimize this for others?
		var vec = new Vector<T>(array.length);
		for (i in 0...array.length)
			vec.set(i, array[i]);
		return vec;
		#end
	}

	/**
		Returns a shallow copy of `this` Vector.

		The elements are not copied and retain their identity, so
		`a[i] == a.copy()[i]` is true for any valid `i`. However,
		`a == a.copy()` is always false.
	**/
	#if cs @:extern #end public inline function copy<T>():Vector<T> {
		var r = new Vector<T>(length);
		Vector.blit(cast this, 0, r, 0, length);
		return r;
	}

	/**
		Returns a string representation of `this` Vector, with `sep` separating
		each element.

		The result of this operation is equal to `Std.string(this[0]) + sep +
		Std.string(this[1]) + sep + ... + sep + Std.string(this[this.length-1])`

		If `this` Vector has length 0, the result is the empty String `""`.
		If `this` has exactly one element, the result is equal to a call to
		`Std.string(this[0])`.

		If `sep` is null, the result is unspecified.
	**/
	#if cs @:extern #end public inline function join<T>(sep:String):String {
		#if (flash10||cpp)
		return this.join(sep);
		#else
		var b = new StringBuf();
		var i = 0;
		var len = length;
		for(i in 0...len) {
			b.add( Std.string(get(i)) );
			if(i < len-1) {
				b.add(sep);
			}
		}
		return b.toString();
		#end
	}

	/**
		Creates a new Vector by applying function `f` to all elements of `this`.

		The order of elements is preserved.

		If `f` is null, the result is unspecified.
	**/
	#if cs @:extern #end public inline function map<S>(f:T->S):Vector<S> {
		var length = length;
		var r = new Vector<S>(length);
		var i = 0;
		var len = length;
		for(i in 0...len) {
			var v = f(get(i));
			r.set(i, v);
		}
		return r;
	}

	/**
		Sorts `this` Vector according to the comparison function `f`, where
		`f(x,y)` returns 0 if x == y, a positive Int if x > y and a
		negative Int if x < y.

		This operation modifies `this` Vector in place.

		The sort operation is not guaranteed to be stable, which means that the
		order of equal elements may not be retained.

		If `f` is null, the result is unspecified.
	**/
	public inline function sort<T>(f:T->T->Int):Void {
		#if (neko || cs || java)
		throw "not yet supported";
		#elseif lua
		haxe.ds.ArraySort.sort(cast this, f);
		#else
		this.sort(f);
		#end
	}
}
