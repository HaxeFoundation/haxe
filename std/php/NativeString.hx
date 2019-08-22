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

package php;

/**
	Native PHP string.
**/
@:coreType @:runtimeValue abstract NativeString from String to String {
	@:arrayAccess function get(key:Int):String;

	@:arrayAccess function set(key:Int, val:String):String;

	public inline function iterator() {
		return new NativeStringIterator(this);
	}

	public inline function keyValueIterator() {
		return new NativeStringKeyValueIterator(this);
	}
}

class NativeStringIterator {
	var s:NativeString;
	var i:Int = 0;

	public inline function new(s:NativeString) {
		this.s = s;
	}

	public inline function hasNext():Bool {
		return i < Global.strlen(s);
	}

	public inline function next():String {
		return s[i++];
	}
}

class NativeStringKeyValueIterator {
	var s:NativeString;
	var i:Int = 0;

	public inline function new(s:NativeString) {
		this.s = s;
	}

	public inline function hasNext():Bool {
		return i < Global.strlen(s);
	}

	public inline function next():{key:Int, value:String} {
		return {key: i, value: s[i++]};
	}
}
