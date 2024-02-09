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

import php.*;
import php.ArrayIterator as NativeArrayIterator;

import haxe.iterators.ArrayKeyValueIterator;

using php.Global;

@:coreApi
final class Array<T> implements ArrayAccess<Int, T> implements IteratorAggregate<T> implements Countable implements JsonSerializable<NativeIndexedArray<T>> {
	public var length(default, null):Int;

	var arr:NativeIndexedArray<T>;

	public function new() {
		arr = new NativeIndexedArray<T>();
		length = 0;
	}

	public function concat(a:Array<T>):Array<T> {
		return wrap(Global.array_merge(arr, a.arr));
	}

	public inline function copy():Array<T> {
		return Syntax.clone(this);
	}

	public inline function filter(f:T->Bool):Array<T> {
		var result = Syntax.arrayDecl();
		for(item in arr) {
			if (f(item)) {
				result.push(item);
			}
		}
		return wrap(result);
	}

	public inline function contains(x:T):Bool {
		return indexOf(x) != -1;
	}

	public function indexOf(x:T, ?fromIndex:Int):Int {
		if (fromIndex == null && !Boot.isHxClosure(x) && !Boot.isNumber(x)) {
			var index = Global.array_search(x, arr, true);
			if (index == false) {
				return -1;
			} else {
				return index;
			}
		}
		if (fromIndex == null) {
			fromIndex = 0;
		} else {
			if (fromIndex < 0)
				fromIndex += length;
			if (fromIndex < 0)
				fromIndex = 0;
		}
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

	@:ifFeature("dynamic_read.iterator", "anon_optional_read.iterator", "anon_read.iterator")
	public inline function iterator():haxe.iterators.ArrayIterator<T> {
		return new haxe.iterators.ArrayIterator(this);
	}

	@:keep
	public inline function keyValueIterator():ArrayKeyValueIterator<T> {
		return new ArrayKeyValueIterator(this);
	}

	public function join(sep:String):String {
		return Global.implode(sep, Global.array_map(Syntax.nativeClassName(Boot) + '::stringify', arr));
	}

	public function lastIndexOf(x:T, ?fromIndex:Int):Int {
		if (fromIndex == null || fromIndex >= length)
			fromIndex = length - 1;
		if (fromIndex < 0)
			fromIndex += length;
		while (fromIndex >= 0) {
			if (arr[fromIndex] == x)
				return fromIndex;
			fromIndex--;
		}
		return -1;
	}

	public inline function map<S>(f:T->S):Array<S> {
		var result = Syntax.arrayDecl();
		for(item in arr) {
			result.push(f(item));
		}
		return wrap(result);
	}

	public inline function pop():Null<T> {
		if (length > 0)
			length--;
		return Global.array_pop(arr);
	}

	public inline function push(x:T):Int {
		arr[length++] = x;
		return length;
	}

	public function remove(x:T):Bool {
		var result = false;
		for(index in 0...length) {
			if (arr[index] == x) {
				Global.array_splice(arr, index, 1);
				length--;
				result = true;
				break;
			}
		}
		return result;
	}

	public inline function reverse():Void {
		arr = Global.array_reverse(arr);
	}

	public inline function shift():Null<T> {
		if (length > 0)
			length--;
		return Global.array_shift(arr);
	}

	public function slice(pos:Int, ?end:Int):Array<T> {
		if (pos < 0)
			pos += length;
		if (pos < 0)
			pos = 0;
		if (end == null) {
			return wrap(Global.array_slice(arr, pos));
		} else {
			if (end < 0)
				end += length;
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
		if (len < 0)
			return [];
		var result = wrap(Global.array_splice(arr, pos, len));
		length -= result.length;
		return result;
	}

	public inline function unshift(x:T):Void {
		length = Global.array_unshift(arr, x);
	}

	public function toString():String {
		return inline Boot.stringifyNativeIndexedArray(arr);
	}

	public function resize(len:Int):Void {
		if (length < len) {
			arr = Global.array_pad(arr, len, null);
		} else if (length > len) {
			Global.array_splice(arr, len, length - len);
		}
		length = len;
	}

	@:noCompletion @:keep
	@:php.attribute('\\ReturnTypeWillChange')
	function offsetExists(offset:Int):Bool {
		return offset < length;
	}

	@:noCompletion @:keep
	@:php.attribute('\\ReturnTypeWillChange')
	function offsetGet(offset:Int):Ref<T> {
		if(offset < 0 || offset >= length) {
			//This var is required in generated php code
			//because only variables can be returned by reference.
			final result = null;
			Syntax.keepVar(result);
			return result;
		}
		return arr[offset];
	}

	@:noCompletion @:keep
	@:php.attribute('\\ReturnTypeWillChange')
	function offsetSet(offset:Int, value:T):Void {
		if (length <= offset) {
			for(i in length...offset + 1) {
				arr[i] = null;
			}
			length = offset + 1;
		}
		arr[offset] = value;
		Syntax.code("return {0}", value);
	}

	@:noCompletion @:keep
	@:php.attribute('\\ReturnTypeWillChange')
	function offsetUnset(offset:Int):Void {
		if (offset >= 0 && offset < length) {
			Global.array_splice(arr, offset, 1);
			--length;
		}
	}

	@:noCompletion @:keep
	@:php.attribute('\\ReturnTypeWillChange')
	private function getIterator():Traversable {
		return new NativeArrayIterator(arr);
	}

	@:noCompletion @:keep
	@:native('count') //to not interfere with `Lambda.count`
	@:php.attribute('\\ReturnTypeWillChange')
	private function _hx_count():Int {
		return length;
	}

	@:noCompletion @:keep
	@:php.attribute('\\ReturnTypeWillChange')
	function jsonSerialize():NativeIndexedArray<T> {
		return arr;
	}

	static function wrap<T>(arr:NativeIndexedArray<T>):Array<T> {
		var a = new Array();
		a.arr = arr;
		a.length = Global.count(arr);
		return a;
	}
}

/**
	Following interfaces are required to make `Array` mimic native arrays for usage
	from a 3rd party PHP code.
**/
@:native('ArrayAccess')
private extern interface ArrayAccess<K, V> {
	private function offsetExists(offset:K):Bool;
	private function offsetGet(offset:K):V;
	private function offsetSet(offset:K, value:V):Void;
	private function offsetUnset(offset:K):Void;
}

@:native('JsonSerializable')
private extern interface JsonSerializable<T> {
	private function jsonSerialize():T;
}

@:native('IteratorAggregate')
private extern interface IteratorAggregate<T> extends Traversable {
	private function getIterator():Traversable;
}

@:native('Countable')
private extern interface Countable {
	@:native('count') //to not interfere with `Lambda.count`
	private function _hx_count():Int;
}