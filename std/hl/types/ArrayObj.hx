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

package hl.types;

import haxe.iterators.ArrayIterator;
import haxe.iterators.ArrayKeyValueIterator;

class ArrayObjIterator<T> extends ArrayIterator<T> {
	var arr:ArrayObj<T>;

	public inline function new(arr:ArrayObj<T>) {
		super((null:Dynamic));
		this.arr = arr;
	}

	override public function hasNext():Bool {
		return current < arr.length;
	}

	override public function next():T {
		return @:privateAccess arr.array[current++];
	}
}

@:keep
class ArrayObj<T> extends ArrayBase {
	var array:hl.NativeArray<Dynamic>;

	public function new() {
		length = 0;
		array = new NativeArray<Dynamic>(0);
	}

	public function concat(a:ArrayObj<T>):ArrayObj<T> {
		var arr = new hl.NativeArray(length + a.length);
		arr.blit(0, array, 0, length);
		arr.blit(length, a.array, 0, a.length);
		return alloc(cast arr);
	}

	override function join(sep:String):String {
		var b = new StringBuf();
		for (i in 0...length) {
			if (i > 0)
				b.add(sep);
			b.add(array[i]);
		}
		return b.toString();
	}

	override function isArrayObj() {
		return true;
	}

	public function pop():Null<T> {
		if (length == 0)
			return null;
		length--;
		var v = array[length];
		array[length] = null;
		return v;
	}

	public function push(x:T):Int {
		var len = length;
		if (array.length == len)
			__expand(len);
		else
			length++;
		array[len] = x;
		return length;
	}

	override function reverse():Void {
		for (i in 0...length >> 1) {
			var k = length - 1 - i;
			var tmp = array[i];
			array[i] = array[k];
			array[k] = tmp;
		}
	}

	public function shift():Null<T> {
		if (length == 0)
			return null;
		length--;
		var v = array[0];
		array.blit(0, array, 1, length);
		array[length] = null;
		return v;
	}

	override function slice(pos:Int, ?end:Int):ArrayObj<T> {
		if (pos < 0) {
			pos = this.length + pos;
			if (pos < 0)
				pos = 0;
		}
		var pend:Int;
		if (end == null)
			pend = this.length;
		else {
			pend = end;
			if (pend < 0)
				pend += this.length;
			if (pend > this.length)
				pend = this.length;
		}
		var len = pend - pos;
		if (len < 0)
			return new ArrayObj();
		return alloc(array.sub(pos, len));
	}

	public function sort(f:T->T->Int):Void {
		// TODO : use native call ?
		haxe.ds.ArraySort.sort(cast this, f);
	}

	override function splice(pos:Int, len:Int):ArrayObj<T> {
		if (len < 0)
			return new ArrayObj();
		if (pos < 0) {
			pos = this.length + pos;
			if (pos < 0)
				pos = 0;
		}
		if (pos > this.length) {
			pos = 0;
			len = 0;
		} else if (pos + len > this.length) {
			len = this.length - pos;
			if (len < 0)
				len = 0;
		}
		var a = this.array;
		var ret:ArrayObj<T> = alloc(cast a.sub(pos, len));
		var end = pos + len;
		a.blit(pos, a, end, this.length - end);
		this.length -= len;
		while (--len >= 0)
			a[this.length + len] = null;
		return ret;
	}

	@:access(Std.toStringDepth)
	override function toString():String {
		if (Std.toStringDepth >= 5)
			return "...";
		Std.toStringDepth++;
		var b = new StringBuf();
		b.addChar("[".code);
		try {
			for (i in 0...length) {
				if (i > 0)
					b.addChar(",".code);
				b.add(array[i]);
			}
		} catch (e:Dynamic) {
			Std.toStringDepth--;
			hl.Api.rethrow(e);
		}
		b.addChar("]".code);
		Std.toStringDepth--;
		return b.toString();
	}

	public function unshift(x:T):Void {
		if (length == array.length)
			__expand(length)
		else
			length++;
		array.blit(1, array, 0, length - 1);
		array[0] = x;
	}

	public function insert(pos:Int, x:T):Void {
		if (pos < 0) {
			pos = length + pos;
			if (pos < 0)
				pos = 0;
		} else if (pos > length)
			pos = length;
		if (length == array.length)
			__expand(length)
		else
			length++;
		array.blit(pos + 1, array, pos, length - pos - 1);
		array[pos] = x;
	}

	public function contains(x:T):Bool {
		return indexOf(x) != -1;
	}

	public function remove(x:T):Bool {
		var i = indexOf(x);
		if (i < 0)
			return false;
		length--;
		array.blit(i, array, i + 1, length - i);
		array[length] = null;
		return true;
	}

	public function indexOf(x:T, ?fromIndex:Int):Int {
		var i:Int = fromIndex;
		if (i < 0) {
			i += length;
			if (i < 0)
				i = 0;
		}
		var length = length;
		var array = array;
		while (i < length) {
			if (array[i] == x)
				return i;
			i++;
		}
		return -1;
	}

	override function blit(pos:Int, src:ArrayBase.ArrayAccess, srcpos:Int, len:Int):Void {
		var src = (cast src : ArrayObj<T>);
		if (pos < 0 || srcpos < 0 || len < 0 || pos + len > length || srcpos + len > src.length)
			throw haxe.io.Error.OutsideBounds;
		array.blit(pos, src.array, srcpos, len);
	}

	public function lastIndexOf(x:T, ?fromIndex:Int):Int {
		var len = length;
		var i:Int = fromIndex != null ? fromIndex : len - 1;
		if (i >= len)
			i = len - 1;
		else if (i < 0)
			i += len;
		while (i >= 0) {
			if (array[i] == x)
				return i;
			i--;
		}
		return -1;
	}

	public function copy():ArrayObj<T> {
		var n = new NativeArray<Dynamic>(length);
		n.blit(0, array, 0, length);
		return alloc(n);
	}

	public function iterator():ArrayIterator<T> {
		return new ArrayObjIterator(this);
	}

	public function keyValueIterator():ArrayKeyValueIterator<T> {
		return new ArrayKeyValueIterator<T>(cast this);
	}

	public function map<S>(f:T->S):ArrayDyn {
		var a = new ArrayObj();
		if (length > 0)
			a.__expand(length - 1);
		for (i in 0...length)
			a.array[i] = f(array[i]);
		return ArrayDyn.alloc(a, true);
	}

	public function filter(f:T->Bool):ArrayObj<T> {
		var a = new ArrayObj();
		for (i in 0...length) {
			var v = array[i];
			if (f(v))
				a.push(v);
		}
		return a;
	}

	override public function resize(len:Int):Void {
		if (length < len) {
			__expand(len - 1);
		} else if (length > len) {
			for (i in len...length) {
				array[i] = null;
			}
			this.length = len;
		}
	}

	// called by compiler when accessing the array outside of its bounds, might trigger resize
	function __expand(index:Int) {
		if (index < 0)
			throw "Invalid array index " + index;
		var newlen = index + 1;
		var size:Int = array.length;
		if (newlen > size) {
			var next = (size * 3) >> 1;
			if (next < newlen)
				next = newlen;
			var arr2 = new hl.NativeArray<Dynamic>(next);
			arr2.blit(0, array, 0, length);
			array = arr2;
		}
		length = newlen;
	}

	override function getDyn(pos:Int):Dynamic {
		var pos:UInt = pos;
		if (pos >= length)
			return null;
		return array[pos];
	}

	override function setDyn(pos:Int, v:Dynamic) {
		var pos:UInt = pos;
		if (pos >= length)
			__expand(pos);
		array[pos] = Api.safeCast(v, array.getType());
	}

	override function pushDyn(v:Dynamic)
		return push(v);

	override function popDyn():Null<Dynamic>
		return pop();

	override function shiftDyn():Null<Dynamic>
		return shift();

	override function unshiftDyn(v:Dynamic)
		unshift(v);

	override function insertDyn(pos:Int, v:Dynamic)
		insert(pos, v);

	override function containsDyn(v:Dynamic)
		return contains(v);

	override function removeDyn(v:Dynamic)
		return remove(v);

	override function sortDyn(f:Dynamic->Dynamic->Int)
		sort(f);

	public static function alloc<T>(a:hl.NativeArray<T>):ArrayObj<T> {
		var arr:ArrayObj<T> = untyped $new(ArrayObj);
		arr.array = a;
		arr.length = a.length;
		return arr;
	}
}
