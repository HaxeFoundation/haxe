/*
 * Copyright (C)2005-2016 Haxe Foundation
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
import php7.NativeIndexedArray;

@:coreApi
@:native("HxArray")
class Array<T> {
	public var length(default, null):Int;
	var arr:NativeIndexedArray<T>;

	public function new() {
		arr = new NativeIndexedArray<T>();
		length = 0;
	}

	public function concat(a:Array<T>):Array<T> {
		return wrap(arr.merge(a.arr));
	}

	public function copy():Array<T> {
		return wrap(arr);
	}

	public function filter(f:T->Bool):Array<T> {
		return wrap(arr.filter(f));
	}

	public function indexOf(x:T, ?fromIndex:Int):Int {
		/*if (fromIndex == null) fromIndex = 0;
		if (fromIndex < 0) fromIndex += length - 1;
		if (fromIndex < 0) fromIndex = 0;
		while (fromIndex < length) {
			if (arr[fromIndex] == x)
				return fromIndex;
			fromIndex++;
		}
		return -1;*/
		// maybe better?
		var idx = arr.search(x);
		if (idx == false)
			return -1;
		if (fromIndex != null) {
			if (fromIndex >= length)
				return -1;
			if (fromIndex < 0) fromIndex += length - 1;
			if (fromIndex > 0) {
				idx -= fromIndex;
				if ((idx:Int) < 0)
					return -1;
			}
		}
		return idx;
	}

	public function insert(pos:Int, x:T):Void {
		length++;
		arr.splice(pos, 0, x);
	}

	public function iterator():Iterator<T> {
		return new ArrayIterator(arr);
	}

	public function join(sep:String):String {
		return arr.implode(sep);
	}

	public function lastIndexOf(x:T, ?fromIndex:Int):Int {
		if (fromIndex == null || fromIndex >= length) fromIndex = length - 1;
		if (fromIndex < 0) fromIndex += length - 1;
		while (fromIndex >= 0) {
			if (arr[fromIndex] == x)
				return fromIndex;
			fromIndex--;
		}
		return -1;
	}

	public function map<S>(f:T->S):Array<S> {
		return wrap(arr.map(f));
	}

	public function pop():Null<T> {
		if (length > 0) length--;
		return arr.pop();
	}

	public function push(x:T):Int {
		return length = arr.push(x);
	}

	public function remove(x:T):Bool {
		for (i in 0...length) {
			if (arr[i] == x) {
				arr.splice(i, 1);
				length--;
				return true;
			}
		}
		return false;
	}

	public function reverse():Void {
		arr = arr.reverse();
	}

	public function shift():Null<T> {
		if (length > 0) length--;
		return arr.shift();
	}

	public function slice(pos:Int, ?end:Int):Array<T> {
		return wrap(arr.slice(pos, end == null ? null : end - pos));
	}

	public function sort(f:T->T->Int):Void {
		arr.usort(f);
	}

	public function splice(pos:Int, len:Int):Array<T> {
		return wrap(arr.splice(pos, len));
	}

	public function unshift(x:T):Void {
		length = arr.unshift(x);
	}

	public function toString():String {
		return "array"; //TODO
	}

	static function wrap<T>(arr:NativeIndexedArray<T>):Array<T> {
		var a = new Array();
		a.arr = arr;
		a.length = arr.count();
		return a;
	}

}

private class ArrayIterator<T> {
	var idx:Int;
	var arr:NativeIndexedArray<T>;

	public inline function new(arr:NativeIndexedArray<T>) {
		arr = this.arr = arr;
		idx = 0;
	}

	public inline function hasNext():Bool {
		return idx < arr.count();
	}

	public inline function next():T {
		return arr[idx++];
	}
}
