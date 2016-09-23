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
package php7;

private class Wrapper {
	public var s : NativeString;
	public inline function new (s:NativeString) {
		this.s = s;
	}
}

abstract BytesData(Wrapper) {

	public var length(get,never):Int;
	inline function get_length() return Global.strlen(str);

	var str (get,set):NativeString;
	inline function get_str() return this.s;
	inline function set_str(v) return this.s = v;

	inline function new (x:Wrapper) this = x;

	static inline function wrap (s:NativeString):Wrapper {
		return new Wrapper(s);
	}

	static inline function ofNativeString (s:NativeString) {
		return new BytesData( wrap(s));
	}

	public inline function get (pos:Int):Int {
		return Global.ord(str[pos]);
	}
 
	public inline function set (index:Int, val:Int):Void {
		str[index] = Global.chr(val);
	}

	public inline function compare (other:BytesData):Int {
		return Global.strcmp(str, other.str);
	}

	public inline function copy ():BytesData {
		return ofNativeString(str);
	}

	public inline function getString (pos:Int, len:Int):String {
		return Global.substr(str, pos, len);
	}

	public inline function sub (pos:Int, len:Int):BytesData {
		return ofString(Global.substr(str, pos, len));
	}

	public inline function blit (pos : Int, src : BytesData, srcpos : Int, len : Int):Void {
		str = (Global.substr(str, 0, pos):String) + (Global.substr(src.str, srcpos, len):String) + (Global.substr(str, pos + len):String);
	}

	public inline function toString():String return str;

	public static inline function ofString (s:String) {
		return ofNativeString(s);
	}

	public static inline function alloc (length:Int) {
		return ofNativeString(Global.str_repeat(Global.chr(0), length));
	}
}