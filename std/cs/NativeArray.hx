/*
 * Copyright (C)2005-2012 Haxe Foundation
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
package cs;

extern class NativeArray<T> extends cs.system.Array implements ArrayAccess<T>
{
	public static function array<T>(elements:haxe.Rest<T>):NativeArray<T>;
	public function new(len:Int):Void;
	public var length(get,never):Int;

	@:extern inline private function get_length():Int return this.Length;

	static function Reverse(arr:cs.system.Array):Void;

	@:extern inline public function iterator():NativeArrayIterator<T>
		return new NativeArrayIterator(this);
}

@:nativeGen private class NativeArrayIterator<T>
{
	public var arr(default,null):NativeArray<T>;
	public var idx(default,null):UInt;

	inline public function new(arr)
	{
		this.arr = arr;
		this.idx = 0;
	}

	inline public function hasNext():Bool
		return this.idx < this.arr.Length;

	inline public function next():T
	{
		return this.arr[this.idx++];
	}
}
