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
 package php;



private class Wrapper {
	public var s : NativeString;
	public inline function new (s:NativeString) {
		this.s = s;
	}
}

abstract BytesData(Wrapper) {

	inline function new (x:Wrapper) this = x;

	inline function str ():php.NativeString return this.s;

	inline function setNativeString (val:NativeString):Void {
		this.s = val;
	}

	inline function get_length ():Int {
		return untyped __call__("strlen", str());
	}

	static inline function wrap (s:NativeString):Wrapper {
		return new Wrapper(s);
	}

	static inline function ofNativeString (s:NativeString) {
		return new BytesData( wrap(s));
	}

	public inline function set (index:Int, val:Int):Void {
		untyped __php__("{0}->s[{1}] = chr({2})", this, index, val);
	}

	public var length(get, never):Int;

	public inline function compare (other:BytesData):Int {
		return untyped __php__("{0} < {1} ? -1 : ({0} == {1} ? 0 : 1)", str(), other.str());
	}

	public inline function get (pos:Int):Int {
		return untyped __call__("ord", str()[pos]);
	}

	public inline function copy ():BytesData {
		return ofNativeString(str());
	}

	public inline function getString (pos:Int, len:Int):String {
		return untyped __call__("substr", str(), pos, len);
	}

	public inline function sub (pos:Int, len:Int):BytesData {
		return ofString(untyped __call__("substr", str(), pos, len));
	}

	public inline function blit (pos : Int, src : BytesData, srcpos : Int, len : Int):Void {
		setNativeString(untyped __php__("substr({0}, 0, {2}) . substr({1}, {3}, {4}) . substr({0}, {2}+{4})", str(), src.str(), pos, srcpos, len));
	}

	public inline function toString():String return cast str();

	public static inline function ofString (s:String) {
		return ofNativeString(cast s);
	}

	public static inline function alloc (length:Int) {
		return ofNativeString(untyped __call__("str_repeat", __call__("chr", 0), length));
	}
}