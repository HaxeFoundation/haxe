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

#if !macro
import python.internal.ArrayImpl;
import python.NativeIterator;
#end
import haxe.iterators.ArrayKeyValueIterator;

@:native("list")
@:coreApi
extern class Array<T> implements ArrayAccess<T> {
	var length(default, null):Int;

	function new():Void;

	inline function concat(a:Array<T>):Array<T> {
		return ArrayImpl.concat(this, a);
	}

	inline function copy():Array<T> {
		return ArrayImpl.copy(this);
	}

	@:runtime inline function iterator():haxe.iterators.ArrayIterator<T> {
		return new haxe.iterators.ArrayIterator(this);
	}

	@:runtime inline public function keyValueIterator():ArrayKeyValueIterator<T> {
		return new ArrayKeyValueIterator(this);
	}

	inline function insert(pos:Int, x:T):Void {
		ArrayImpl.insert(this, pos, x);
	}

	@:runtime inline function join(sep:String):String {
		return ArrayImpl.join(this, sep);
	}

	inline function toString():String {
		return ArrayImpl.toString(this);
	}

	@:runtime inline function pop():Null<T> {
		return ArrayImpl.pop(this);
	}

	@:runtime inline function push(x:T):Int {
		return ArrayImpl.push(this, x);
	}

	inline function unshift(x:T):Void {
		ArrayImpl.unshift(this, x);
	}

	inline function indexOf(x:T, ?fromIndex:Int):Int {
		return ArrayImpl.indexOf(this, x, fromIndex);
	}

	inline function lastIndexOf(x:T, ?fromIndex:Int):Int {
		return ArrayImpl.lastIndexOf(this, x, fromIndex);
	}

	inline function remove(x:T):Bool {
		return ArrayImpl.remove(this, x);
	}

	inline function contains(x:T):Bool {
		return ArrayImpl.contains(this,x);
	}

	inline function reverse():Void {
		ArrayImpl.reverse(this);
	}

	@:runtime inline function shift():Null<T> {
		return ArrayImpl.shift(this);
	}

	inline function slice(pos:Int, ?end:Int):Array<T> {
		return ArrayImpl.slice(this, pos, end);
	}

	inline function sort(f:T->T->Int):Void {
		ArrayImpl.sort(this, f);
	}

	inline function splice(pos:Int, len:Int):Array<T> {
		return ArrayImpl.splice(this, pos, len);
	}

	@:runtime inline function map<S>(f:T->S):Array<S> {
		return ArrayImpl.map(this, f);
	}

	@:runtime inline function filter(f:T->Bool):Array<T> {
		return ArrayImpl.filter(this, f);
	}

	inline function resize(len:Int):Void {
		ArrayImpl.resize(this, len);
	}

	@:keep private inline function _get(idx:Int):T {
		return ArrayImpl._get(this, idx);
	}

	@:keep private inline function _set(idx:Int, val:T):T {
		return ArrayImpl._set(this, idx, val);
	}

	@:keep private inline function unsafeGet(idx:Int):T {
		return ArrayImpl.unsafeGet(this, idx);
	}

	@:keep private inline function unsafeSet(idx:Int, val:T):T {
		return ArrayImpl.unsafeSet(this, idx, val);
	}

	@:noCompletion private function __iter__():NativeIterator<T>;
}
