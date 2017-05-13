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
import java.lang.System;
import java.NativeArray;

@:classCode('
	public Array(T[] _native)
	{
		this.__a = _native;
		this.length = _native.length;
	}
')
@:final @:coreApi class Array<T> implements ArrayAccess<T> {

	public var length(default,null) : Int;

	private var __a:NativeArray<T>;

	@:functionCode('
			return new Array<X>(_native);
	')
	private static function ofNative<X>(native:NativeArray<X>):Array<X>
	{
		return null;
	}

	@:functionCode('
			return new Array<Y>((Y[]) ((java.lang.Object)new java.lang.Object[size]));
	')
	private static function alloc<Y>(size:Int):Array<Y>
	{
		return null;
	}

	public function new() : Void
	{
		this.length = 0;
		this.__a = new NativeArray(0);
	}

	public function concat( a : Array<T> ) : Array<T>
	{
		var length = length;
		var len = length + a.length;
		var retarr = new NativeArray(len);
		System.arraycopy(__a, 0, retarr, 0, length);
		System.arraycopy(a.__a, 0, retarr, length, a.length);

		return ofNative(retarr);
	}

	private function concatNative( a : NativeArray<T> ) : Void
	{
		var __a = __a;
		var length = length;
		var len = length + a.length;
		if (__a.length >= len)
		{
			System.arraycopy(a, 0, __a, length, length);
		} else {
			var newarr = new NativeArray(len);
			System.arraycopy(__a, 0, newarr, 0, length);
			System.arraycopy(a, 0, newarr, length, a.length);

			this.__a = newarr;
		}

		this.length = len;
	}

	public function join( sep : String ) : String
	{
		var buf = new StringBuf();
		var i = -1;

		var first = true;
		var length = length;
		while (++i < length)
		{
			if (first)
				first = false;
			else
				buf.add(sep);
			buf.add(__a[i]);
		}

		return buf.toString();
	}

	public function pop() : Null<T>
	{
		var __a = __a;
		var length = length;
		if (length > 0)
		{
			var val = __a[--length];
			__a[length] = null;
			this.length = length;

			return val;
		} else {
			return null;
		}
	}

	public function push(x : T) : Int
	{
		var length = length;
		if (length >= __a.length)
		{
			var newLen = (length << 1) + 1;
			var newarr = new NativeArray(newLen);
			System.arraycopy(__a, 0, newarr, 0, __a.length);

			this.__a = newarr;
		}

		__a[length] = x;
		return ++this.length;
	}

	public function reverse() : Void
	{
		var i = 0;
		var l = this.length;
		var a = this.__a;
		var half = l >> 1;
		l -= 1;
		while ( i < half )
		{
			var tmp = a[i];
			a[i] = a[l-i];
			a[l-i] = tmp;
			i += 1;
		}
	}

	public function shift() : Null<T>
	{
		var l = this.length;
		if( l == 0 )
			return null;

		var a = this.__a;
		var x = a[0];
		l -= 1;
		System.arraycopy(a, 1, a, 0, length-1);
		a[l] = null;
		this.length = l;

		return x;
	}

	public function slice( pos : Int, ?end : Int ) : Array<T>
	{
		if( pos < 0 ){
			pos = this.length + pos;
			if( pos < 0 )
				pos = 0;
		}
		if( end == null )
			end = this.length;
		else if( end < 0 )
			end = this.length + end;
		if( end > this.length )
			end = this.length;
		var len = end - pos;
		if ( len < 0 ) return new Array();

		var newarr = new NativeArray(len);
		System.arraycopy(__a, pos, newarr, 0, len);

		return ofNative(newarr);
	}

	public function sort( f : T -> T -> Int ) : Void
	{
		if (length == 0)
			return;
		quicksort(0, length - 1, f);
	}

	private function quicksort( lo : Int, hi : Int, f : T -> T -> Int ) : Void
	{
		var buf = __a;
		var i = lo, j = hi;
		var p = buf[(i + j) >> 1];
		while ( i <= j )
		{
			while ( i < hi && f(buf[i], p) < 0 ) i++;
			while ( j > lo && f(buf[j], p) > 0 ) j--;
			if ( i <= j )
			{
				var t = buf[i];
				buf[i++] = buf[j];
				buf[j--] = t;
			}
		}

		if( lo < j ) quicksort( lo, j, f );
		if( i < hi ) quicksort( i, hi, f );
	}

	public function splice( pos : Int, len : Int ) : Array<T>
	{
		if( len < 0 ) return new Array();
		if( pos < 0 ) {
			pos = this.length + pos;
			if( pos < 0 ) pos = 0;
		}
		if( pos > this.length ) {
			pos = 0;
			len = 0;
		} else if( pos + len > this.length ) {
			len = this.length - pos;
			if( len < 0 ) len = 0;
		}
		var a = this.__a;

		var ret = new NativeArray(len);
		System.arraycopy(a, pos, ret, 0, len);
		var ret = ofNative(ret);

		var end = pos + len;
		System.arraycopy(a, end, a, pos, this.length - end);
		this.length -= len;
		while( --len >= 0 )
			a[this.length + len] = null;
		return ret;
	}

	private function spliceVoid( pos : Int, len : Int ) : Void
	{
		if( len < 0 ) return;
		if( pos < 0 ) {
			pos = this.length + pos;
			if( pos < 0 ) pos = 0;
		}
		if( pos > this.length ) {
			pos = 0;
			len = 0;
		} else if( pos + len > this.length ) {
			len = this.length - pos;
			if( len < 0 ) len = 0;
		}
		var a = this.__a;

		var end = pos + len;
		System.arraycopy(a, end, a, pos, this.length - end);
		this.length -= len;
		while( --len >= 0 )
			a[this.length + len] = null;
	}

	public function toString() : String
	{
		var ret = new StringBuf();
		var a = __a;
		ret.add("[");
		var first = true;
		for (i in 0...length)
		{
			if (first)
				first = false;
			else
				ret.add(",");
			ret.add(a[i]);
		}

		ret.add("]");
		return ret.toString();
	}

	public function unshift( x : T ) : Void
	{
		var __a = __a;
		var length = length;
		if (length >= __a.length)
		{
			var newLen = (length << 1) + 1;
			var newarr = new NativeArray(newLen);
			System.arraycopy(__a, 0, newarr, 1, length);

			this.__a = newarr;
		} else {
			System.arraycopy(__a, 0, __a, 1, length);
		}

		this.__a[0] = x;
		++this.length;
	}

	public function insert( pos : Int, x : T ) : Void
	{
		var l = this.length;
		if( pos < 0 ) {
			pos = l + pos;
			if( pos < 0 ) pos = 0;
		}
		if ( pos >= l ) {
			this.push(x);
			return;
		} else if (pos == 0) {
			this.unshift(x);
			return;
		}

		if (l >= __a.length)
		{
			var newLen = (length << 1) + 1;
			var newarr = new NativeArray(newLen);
			System.arraycopy(__a, 0, newarr, 0, pos);
			newarr[pos] = x;
			System.arraycopy(__a, pos, newarr, pos + 1, l - pos);

			this.__a = newarr;
			++this.length;
		} else {
			var __a = __a;
			System.arraycopy(__a, pos, __a, pos + 1, l - pos);
			System.arraycopy(__a, 0, __a, 0, pos);
			__a[pos] = x;
			++this.length;
		}
	}

	public function remove( x : T ) : Bool
	{
		var __a = __a;
		var i = -1;
		var length = length;
		while (++i < length)
		{
			if (__a[i] == x)
			{
				System.arraycopy(__a, i + 1, __a, i, length - i - 1);
				__a[--this.length] = null;

				return true;
			}
		}

		return false;
	}

	public function indexOf(x : T, ?fromIndex:Int) : Int {
		var len = length, a = __a, i:Int = (fromIndex == null) ? 0 : fromIndex;
		if (i < 0)
		{
			i += len;
			if (i < 0) i = 0;
		}
		while (i < len)
		{
			if (a[i] == x)
				return i;
			i++;
		}
		return -1;
	}

	public function lastIndexOf(x : T, ?fromIndex:Int) : Int {
		var len = length, a = __a, i:Int = (fromIndex == null) ? len - 1 : fromIndex;
		if (i >= len)
			i = len - 1;
		else if (i < 0)
			i += len;
		while (i >= 0)
		{
			if (a[i] == x)
				return i;
			i--;
		}
		return -1;
	}

	public function copy() : Array<T>
	{
		var len = length;
		var __a = __a;
		var newarr = new NativeArray(len);
		System.arraycopy(__a, 0, newarr, 0, len);
		return ofNative(newarr);
	}

	public inline function iterator() : Iterator<T>
	{
		return new ArrayIterator<T>(this);
	}

	public function map<S>( f : T -> S ) : Array<S> {
		var ret = [];
		for (elt in this)
			ret.push(f(elt));
		return ret;
	}

	public function filter( f : T -> Bool ) : Array<T> {
		var ret = [];
		for (elt in this)
			if (f(elt))
				ret.push(elt);
		return ret;
	}

	private function __get(idx:Int):T
	{
		var __a = __a;
		if (idx >= __a.length || idx < 0)
			return null;

		return __a[idx];
	}

	private function __set(idx:Int, v:T):T
	{
		var __a = __a;
		if (idx >= __a.length)
		{
			var newl = idx + 1;
			if (idx == __a.length)
				newl = (idx << 1) + 1;
			var newArr = new NativeArray<T>(newl);
			if (length > 0)
				System.arraycopy(__a, 0, newArr, 0, length);
			this.__a = __a = newArr;
		}

		if (idx >= length)
			this.length = idx + 1;

		return __a[idx] = v;
	}

	private inline function __unsafe_get(idx:Int):T
	{
		return __a[idx];
	}

	private inline function __unsafe_set(idx:Int, val:T):T
	{
		return __a[idx] = val;
	}
}

@:final
private class ArrayIterator<T>
{
	var arr:Array<T>;
	var len:Int;
	var i:Int;

	public inline function new(a:Array<T>)
	{
		arr = a;
		len = a.length;
		i = 0;
	}

	public inline function hasNext():Bool return i < len;
	public inline function next():T return arr[i++];
}
