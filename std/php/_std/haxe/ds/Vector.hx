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

package haxe.ds;

import php.*;

private class PhpVectorData<T> {
	public var length:Int;
	public var data:NativeIndexedArray<T>;

	public inline function new(length:Int, data:NativeIndexedArray<T>) {
		this.length = length;
		this.data = data;
	}
}

private typedef VectorData<T> = PhpVectorData<T>;

@:coreApi
abstract Vector<T>(VectorData<T>) {
	public var length(get, never):Int;

	extern overload public inline function new(length:Int) {
		this = new VectorData(length, new NativeIndexedArray());
	}

	extern overload public inline function new(length:Int, defaultValue:T):Vector<T> {
		this = new VectorData(length, Global.array_fill(0, length, defaultValue));
	}

	@:op([]) public inline function get(index:Int):T {
		return Syntax.coalesce(this.data[index], null);
	}

	@:op([]) public inline function set(index:Int, val:T):T {
		return this.data[index] = val;
	}

	inline function get_length():Int {
		return this.length;
	}

	public inline function fill(value:T):Void
		this.data = Global.array_fill(0, length, value);

	public static function blit<T>(src:Vector<T>, srcPos:Int, dest:Vector<T>, destPos:Int, len:Int):Void {
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
	}

	public function toArray():Array<T> {
		var result = [];
		@:privateAccess result.length = length;
		for (i in 0...length) {
			@:privateAccess result.arr.push(get(i));
		}
		return result;
	}

	public inline function toData():VectorData<T> {
		return this;
	}

	static public inline function fromData<T>(data:VectorData<T>):Vector<T> {
		return cast data;
	}

	static public inline function fromArrayCopy<T>(array:Array<T>):Vector<T> {
		var vectorData = new VectorData(array.length, @:privateAccess array.arr);
		return cast vectorData;
	}

	public inline function copy<T>():Vector<T> {
		return cast Syntax.clone(this);
	}

	public function join<T>(sep:String):String {
		if (this.length == 0) {
			return '';
		}
		var result = Std.string(get(0));
		for (i in 1...this.length) {
			result = Syntax.concat(result, Syntax.concat(sep, Std.string(get(i))));
		}
		return result;
	}

	public inline function map<S>(f:T->S):Vector<S> {
		var result = new Vector(this.length);
		Syntax.foreach(this.data, function(key:Int, value:T) {
			result[key] = f(value);
		});
		return result;
	}

	public inline function sort(f:T->T->Int):Void {
		Global.usort(this.data, f);
	}
}
