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
import php.*;

using php.Global;

@:coreApi
@:final
class Array<T> implements ArrayAccess<Int,T> {
	public var length(default, null):Int;
	var arr:NativeIndexedArray<T>;

	public function new() {
		arr = new NativeIndexedArray<T>();
		length = 0;
	}

	public function concat(a:Array<T>):Array<T> {
		return wrap(Global.array_merge(arr, a.arr));
	}

	public function copy():Array<T> {
		return wrap(arr);
	}

	public function filter(f:T->Bool):Array<T> {
		return wrap(Global.array_values(Global.array_filter(arr, f)));
	}

	public function indexOf(x:T, ?fromIndex:Int):Int {
		if (fromIndex == null) {
			var index = Global.array_search(x, arr, true);
			if (index == false) {
				return -1;
			} else {
				return index;
			}
		}
		if (fromIndex < 0) fromIndex += length;
		if (fromIndex < 0) fromIndex = 0;
		while (fromIndex < length) {
			if (arr[fromIndex] == x)
				return fromIndex;
			fromIndex++;
		}
		return -1;
	}

	public function insert(pos:Int, x:T):Void {
		length++;
		Global.array_splice(arr, pos, 0, Syntax.arrayDecl(x));
	}

	@:keep
	public function iterator() : Iterator<T> {
		return new ArrayIterator(this);
	}

	public function join(sep:String):String {
		return Global.implode(sep, Global.array_map('strval', arr));
	}

	public function lastIndexOf(x:T, ?fromIndex:Int):Int {
		if (fromIndex == null || fromIndex >= length) fromIndex = length - 1;
		if (fromIndex < 0) fromIndex += length;
		while (fromIndex >= 0) {
			if (arr[fromIndex] == x)
				return fromIndex;
			fromIndex--;
		}
		return -1;
	}

	public inline function map<S>(f:T->S):Array<S> {
		var result = Syntax.arrayDecl();
		for(i in 0...length) {
			result[i] = f(arr[i]);
		}
		return wrap(result);
	}

	public inline function pop():Null<T> {
		if (length > 0) length--;
		return Global.array_pop(arr);
	}

	public inline function push(x:T):Int {
		arr[length] = x;
		return ++length;
	}

	public function remove(x:T):Bool {
		for (i in 0...length) {
			if (arr[i] == x) {
				Global.array_splice(arr, i, 1);
				length--;
				return true;
			}
		}
		return false;
	}

	public inline function reverse():Void {
		arr = Global.array_reverse(arr);
	}

	public inline function shift():Null<T> {
		if (length > 0) length--;
		return Global.array_shift(arr);
	}

	public function slice(pos:Int, ?end:Int):Array<T> {
		if (pos < 0) pos += length;
		if (pos < 0) pos = 0;
		if (end == null) {
			return wrap(Global.array_slice(arr, pos));
		} else {
			if (end < 0) end += length;
			if (end <= pos) {
				return [];
			} else {
				return wrap(Global.array_slice(arr, pos, end - pos));
			}
		}
	}

	public inline function sort(f:T->T->Int):Void {
		arr.usort(f);
	}

	public function splice(pos:Int, len:Int):Array<T> {
		if (len < 0) return [];
		var result = wrap(Global.array_splice(arr, pos, len));
		length -= result.length;
		return result;
	}

	public inline function unshift(x:T):Void {
		length = Global.array_unshift(arr, x);
	}

	public function toString():String {
		var strings = Syntax.arrayDecl();
		Syntax.foreach(arr, function(index:Int, value:T) {
			strings[index] = Boot.stringify(value);
		});
		return '[' + Global.implode(',', strings) + ']';
	}

	@:noCompletion
	function offsetExists( offset:Int ) : Bool {
		return offset < length;
	}

	@:noCompletion
	function offsetGet( offset:Int ) : Ref<T> {
		try {
			return arr[offset];
		} catch(e:Dynamic) {
			return null;
		}
	}

	@:noCompletion
	function offsetSet( offset:Int, value:T ) : Void {
		if (length <= offset) {
			arr = Global.array_merge(arr, Global.array_fill(0, offset + 1 - length, null));
			length = offset + 1;
		}
		arr[offset] = value;
	}

	@:noCompletion
	function offsetUnset( offset:Int ) : Void {
		if (offset >= 0 && offset < length ) {
			Global.array_splice(arr, offset, 1);
			--length;
		}
	}

	static function wrap<T>(arr:NativeIndexedArray<T>):Array<T> {
		var a = new Array();
		a.arr = arr;
		a.length = Global.count(arr);
		return a;
	}
}

private class ArrayIterator<T> {
	var idx:Int;
	var arr:Array<T>;

	public inline function new(arr:Array<T>) {
		this.arr = arr;
		idx = 0;
	}

	public inline function hasNext():Bool {
		return idx < arr.length;
	}

	public inline function next():T {
		return arr[idx++];
	}

	@:keep
	@:phpMagic
	function __get(method:String) {
		return switch(method) {
			case 'hasNext', 'next': Boot.closure(this, method);
			case _: null;
		}
	}
}


/**
	This one is required for `Array`
**/
@:native('ArrayAccess')
private extern interface ArrayAccess<K,V> {
	private function offsetExists( offset:K ) : Bool;
	private function offsetGet( offset:K ) : V;
	private function offsetSet( offset:K, value:V ) : Void;
	private function offsetUnset( offset:K ) : Void;
}