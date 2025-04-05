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

@:coreApi
extern class Array<T> {
	var length(default, null):Int;

	function new():Void;
	function concat(a:Array<T>):Array<T>;
	function join(sep:String):String;
	function pop():Null<T>;
	function push(x:T):Int;
	function reverse():Void;
	function shift():Null<T>;
	function slice(pos:Int, ?end:Int):Array<T>;
	function sort(f:T->T->Int):Void;
	function splice(pos:Int, len:Int):Array<T>;
	function toString():String;
	function unshift(x:T):Void;

	inline function insert(pos:Int, x:T):Void {
		(cast this).splice(pos, 0, x);
	}

	inline function remove(x:T):Bool {
		return @:privateAccess HxOverrides.remove(this, x);
	}

	inline function contains(x:T):Bool {
		#if (js_es >= 6)
		return (cast this).includes(x);
		#else
		return this.indexOf(x) != -1;
		#end
	}

	#if (js_es >= 5)
	@:pure function indexOf(x:T, ?fromIndex:Int):Int;
	@:pure function lastIndexOf(x:T, ?fromIndex:Int):Int;
	#else
	inline function indexOf(x:T, ?fromIndex:Int):Int {
		return @:privateAccess HxOverrides.indexOf(this, x, (fromIndex != null) ? fromIndex : 0);
	}

	inline function lastIndexOf(x:T, ?fromIndex:Int):Int {
		return @:privateAccess HxOverrides.lastIndexOf(this, x, (fromIndex != null) ? fromIndex : length - 1);
	}
	#end

	@:pure
	inline function copy():Array<T> {
		return (cast this).slice();
	}

	@:runtime inline function map<S>(f:T->S):Array<S> {
		var result:Array<S> = js.Syntax.construct(Array, length);
		for(i in 0...length) {
			result[i] = f(this[i]);
		}
		return result;
	}

	@:runtime inline function filter(f:T->Bool):Array<T> {
		return [for (v in this) if (f(v)) v];
	}

	@:runtime inline function iterator():haxe.iterators.ArrayIterator<T> {
		return new haxe.iterators.ArrayIterator(this);
	}

	@:runtime inline function keyValueIterator():ArrayKeyValueIterator<T> {
		return new ArrayKeyValueIterator(this);
	}

	inline function resize(len:Int):Void {
		this.length = len;
	}
}
