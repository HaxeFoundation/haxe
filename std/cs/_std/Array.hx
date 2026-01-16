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

import haxe.iterators.ArrayKeyValueIterator;

@:coreApi final class Array<T> implements ArrayAccess<T> {
	public var length(default, null):Int;

	private var __a:cs.NativeArray<T>;

	@:skipReflection static var __hx_toString_depth = 0;
	@:skipReflection static inline final __hx_defaultCapacity = 4;

	private static function ofNative<X>(native:cs.NativeArray<X>):Array<X> {
		var a = new Array<X>();
		a.length = native.length;
		a.__a = native;
		return a;
	}

	private static function alloc<Y>(size:Int):Array<Y> {
		var a = new Array<Y>();
		a.length = size;
		a.__a = new cs.NativeArray<Y>(size);
		return a;
	}

	public function new():Void {
		this.length = 0;
		this.__a = new cs.NativeArray<T>(0);
	}

	public function concat(a:Array<T>):Array<T> {
		var len = length + a.length;
		var retarr = new cs.NativeArray<T>(len);
		cs.NativeArray.arraycopy(__a, 0, retarr, 0, length);
		cs.NativeArray.arraycopy(a.__a, 0, retarr, length, a.length);
		return ofNative(retarr);
	}

	public function join(sep:String):String {
		var buf = new StringBuf();
		var first = true;
		for (i in 0...length) {
			if (first)
				first = false;
			else
				buf.add(sep);
			buf.add(__a[i]);
		}
		return buf.toString();
	}

	public function pop():Null<T> {
		if (length > 0) {
			var val = __a[--length];
			__a[length] = null;
			return val;
		} else {
			return null;
		}
	}

	public function push(x:T):Int {
		if (length >= __a.length) {
			var newLen = length == 0 ? __hx_defaultCapacity : (length << 1);
			var newarr = new cs.NativeArray<T>(newLen);
			cs.NativeArray.arraycopy(__a, 0, newarr, 0, __a.length);
			__a = newarr;
		}
		__a[length] = x;
		return ++length;
	}

	public function reverse():Void {
		var i = 0;
		var l = length;
		var half = l >> 1;
		l -= 1;
		while (i < half) {
			var tmp = __a[i];
			__a[i] = __a[l - i];
			__a[l - i] = tmp;
			i += 1;
		}
	}

	public function shift():Null<T> {
		if (length == 0)
			return null;

		var x = __a[0];
		length -= 1;
		cs.NativeArray.arraycopy(__a, 1, __a, 0, length);
		__a[length] = null;
		return x;
	}

	public function slice(pos:Int, ?end:Int):Array<T> {
		if (pos < 0) {
			pos = length + pos;
			if (pos < 0)
				pos = 0;
		}
		if (end == null)
			end = length;
		else if (end < 0)
			end = length + end;
		if (end > length)
			end = length;
		var len = end - pos;
		if (len < 0)
			return new Array();

		var newarr = new cs.NativeArray<T>(len);
		cs.NativeArray.arraycopy(__a, pos, newarr, 0, len);
		return ofNative(newarr);
	}

	public function sort(f:T->T->Int):Void {
		if (length == 0)
			return;
		quicksort(0, length - 1, f);
	}

	private function quicksort(lo:Int, hi:Int, f:T->T->Int):Void {
		var i = lo, j = hi;
		var p = __a[(i + j) >> 1];
		while (i <= j) {
			while (i < hi && f(__a[i], p) < 0)
				i++;
			while (j > lo && f(__a[j], p) > 0)
				j--;
			if (i <= j) {
				var t = __a[i];
				__a[i++] = __a[j];
				__a[j--] = t;
			}
		}
		if (lo < j)
			quicksort(lo, j, f);
		if (i < hi)
			quicksort(i, hi, f);
	}

	public function splice(pos:Int, len:Int):Array<T> {
		if (len < 0)
			return new Array();
		if (pos < 0) {
			pos = length + pos;
			if (pos < 0)
				pos = 0;
		}
		if (pos > length) {
			pos = 0;
			len = 0;
		} else if (pos + len > length) {
			len = length - pos;
			if (len < 0)
				len = 0;
		}

		var ret = new cs.NativeArray<T>(len);
		cs.NativeArray.arraycopy(__a, pos, ret, 0, len);

		var end = pos + len;
		cs.NativeArray.arraycopy(__a, end, __a, pos, length - end);
		length -= len;
		while (--len >= 0)
			__a[length + len] = null;
		return ofNative(ret);
	}

	public function toString():String {
		if (__hx_toString_depth >= 5)
			return "...";
		++__hx_toString_depth;
		try {
			var s = __hx_toString();
			--__hx_toString_depth;
			return s;
		} catch (e:Dynamic) {
			--__hx_toString_depth;
			throw(e);
		}
	}

	function __hx_toString():String {
		var ret = new StringBuf();
		ret.add("[");
		var first = true;
		for (i in 0...length) {
			if (first)
				first = false;
			else
				ret.add(",");
			ret.add(__a[i]);
		}
		ret.add("]");
		return ret.toString();
	}

	public function unshift(x:T):Void {
		if (length >= __a.length) {
			var newLen = (length << 1) + 1;
			var newarr = new cs.NativeArray<T>(newLen);
			cs.NativeArray.arraycopy(__a, 0, newarr, 1, length);
			__a = newarr;
		} else {
			cs.NativeArray.arraycopy(__a, 0, __a, 1, length);
		}
		__a[0] = x;
		++length;
	}

	public function insert(pos:Int, x:T):Void {
		if (pos < 0) {
			pos = length + pos;
			if (pos < 0)
				pos = 0;
		}
		if (pos >= length) {
			push(x);
			return;
		} else if (pos == 0) {
			unshift(x);
			return;
		}

		if (length >= __a.length) {
			var newLen = (length << 1) + 1;
			var newarr = new cs.NativeArray<T>(newLen);
			cs.NativeArray.arraycopy(__a, 0, newarr, 0, pos);
			newarr[pos] = x;
			cs.NativeArray.arraycopy(__a, pos, newarr, pos + 1, length - pos);
			__a = newarr;
			++length;
		} else {
			cs.NativeArray.arraycopy(__a, pos, __a, pos + 1, length - pos);
			__a[pos] = x;
			++length;
		}
	}

	public function remove(x:T):Bool {
		for (i in 0...length) {
			if (__a[i] == x) {
				cs.NativeArray.arraycopy(__a, i + 1, __a, i, length - i - 1);
				__a[--length] = null;
				return true;
			}
		}
		return false;
	}

	public function contains(x:T):Bool {
		for (i in 0...length) {
			if (__a[i] == x)
				return true;
		}
		return false;
	}

	public function indexOf(x:T, ?fromIndex:Int):Int {
		var i:Int = (fromIndex == null) ? 0 : fromIndex;
		if (i < 0) {
			i += length;
			if (i < 0)
				i = 0;
		}
		while (i < length) {
			if (__a[i] == x)
				return i;
			i++;
		}
		return -1;
	}

	public function lastIndexOf(x:T, ?fromIndex:Int):Int {
		var i:Int = (fromIndex == null) ? length - 1 : fromIndex;
		if (i >= length)
			i = length - 1;
		else if (i < 0)
			i += length;
		while (i >= 0) {
			if (__a[i] == x)
				return i;
			i--;
		}
		return -1;
	}

	public function copy():Array<T> {
		var newarr = new cs.NativeArray<T>(length);
		cs.NativeArray.arraycopy(__a, 0, newarr, 0, length);
		return ofNative(newarr);
	}

	public inline function iterator():haxe.iterators.ArrayIterator<T> {
		return new haxe.iterators.ArrayIterator<T>(this);
	}

	public inline function keyValueIterator():ArrayKeyValueIterator<T> {
		return new ArrayKeyValueIterator<T>(this);
	}

	public function resize(len:Int):Void {
		if (len == 0) {
			length = 0;
			__a = new cs.NativeArray<T>(0);
		} else if (length < len) {
			if (__a.length < len) {
				var newArr = new cs.NativeArray<T>(len);
				if (length > 0)
					cs.NativeArray.arraycopy(__a, 0, newArr, 0, length);
				__a = newArr;
			}
			length = len;
		} else if (length > len) {
			for (i in len...length)
				__a[i] = null;
			length = len;
		}
	}

	public inline function map<S>(f:T->S):Array<S> {
		var ret = alloc(length);
		for (i in 0...length)
			ret.__set(i, f(__get(i)));
		return ret;
	}

	public inline function filter(f:T->Bool):Array<T> {
		var ret = [];
		for (i in 0...length) {
			var elt = __get(i);
			if (f(elt))
				ret.push(elt);
		}
		return ret;
	}

	private function __get(idx:Int):T {
		if (idx >= __a.length || idx < 0)
			return null;
		return __a[idx];
	}

	private function __set(idx:Int, v:T):Void {
		if (idx >= __a.length) {
			var newl = idx + 1;
			if (idx == __a.length)
				newl = (idx << 1) + 1;
			var newArr = new cs.NativeArray<T>(newl);
			if (length > 0)
				cs.NativeArray.arraycopy(__a, 0, newArr, 0, length);
			__a = newArr;
		}
		if (idx >= length)
			length = idx + 1;
		__a[idx] = v;
	}

	private inline function __unsafe_get(idx:Int):T {
		return __a[idx];
	}

	private inline function __unsafe_set(idx:Int, val:T):T {
		return __a[idx] = val;
	}
}
