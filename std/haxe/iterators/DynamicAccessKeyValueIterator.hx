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

package haxe.iterators;

/**
	This Key/Value iterator can be used to iterate over `haxe.DynamicAccess`.
**/
class DynamicAccessKeyValueIterator<T> {
	final access:DynamicAccess<T>;
	final keys:Array<String>;
	var index:Int;

	public inline function new(access:DynamicAccess<T>) {
		this.access = access;
		this.keys = access.keys();
		index = 0;
	}

	/**
		See `Iterator.hasNext`
	**/
	public inline function hasNext():Bool {
		return index < keys.length;
	}

	/**
		See `Iterator.next`
	**/
	public inline function next():{key:String, value:T} {
		var key = keys[index++];
		return {value: (access[key] : T), key: key};
	}
}
