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

package cpp;

@:include("stdlib.h")
extern class Native {
	@:native("malloc")
	public static function nativeMalloc(bytes:Int):cpp.Star<cpp.Void>;
	@:native("calloc")
	public static function nativeCalloc(bytes:Int):cpp.Star<cpp.Void>;
	@:native("realloc")
	public static function nativeRealloc(inPtr:cpp.Star<cpp.Void>, bytes:Int):cpp.RawPointer<cpp.Void>;
	@:native("free")
	public static function nativeFree(ptr:cpp.Star<cpp.Void>):Void;
	@:native("memcpy")
	public static function nativeMemcpy(dest:cpp.Star<cpp.Void>, src:cpp.Star<cpp.Void>, bytes:Int):Void;

	@:native("hx::ClassSizeOf") @:templatedCall
	public static function sizeof<T>(t:T):Int;

	#if !cppia
	@:native("hx::Dereference")
	public static function star<T>(ptr:cpp.Star<T>):cpp.Reference<T>;

	@:generic
	public static inline function set<T>(ptr:cpp.Star<T>, value:T):Void {
		var ref:cpp.Reference<T> = star(ptr);
		ref = value;
	}
	@:generic
	public static inline function get<T>(ptr:cpp.Star<T>):T {
		var ref:cpp.Reference<T> = star(ptr);
		return ref;
	}

	@:generic
	public static inline function memcpy<DEST, SRC>(dest:cpp.Star<DEST>, src:cpp.Star<SRC>, bytes:Int):Void
		nativeMemcpy(cast dest, cast src, bytes);

	@:generic
	public static inline function malloc<T>(bytes:Int):cpp.Star<T>
		return cast nativeMalloc(bytes);

	@:generic
	public static inline function calloc<T>(bytes:Int):cpp.Star<T>
		return cast nativeCalloc(bytes);

	@:generic
	public static inline function realloc<T>(ioPtr:cpp.Star<T>, bytes:Int):cpp.Star<T>
		return cast nativeRealloc(cast ioPtr, bytes);

	@:generic
	public static inline function free<T>(ptr:cpp.Star<T>):Void {
		if (ptr != null)
			nativeFree(cast ptr);
	}

	@:native("hx::StarOf")
	public static function addressOf<T>(inVariable:Reference<T>):Star<T>;
	#else
	public static inline function addressOf<T>(inVariable:Reference<T>):Star<T> {
		throw "Native.addressOf not available in cppia";
	}
	public static inline function star<T>(ptr:cpp.Star<T>):cpp.Reference<T> {
		throw "Native.star not available in cppia";
	}

	public static inline function set<T>(ptr:cpp.Star<T>, value:T):Void {
		throw "Native.set not available in cppia";
	}
	public static inline function get<T>(ptr:cpp.Star<T>):T {
		throw "Native.get not available in cppia";
		var d:Dynamic = null;
		return d;
	}

	public static function memcpy<DEST, SRC>(dest:cpp.Star<DEST>, src:cpp.Star<SRC>, bytes:Int):Void;
	public static function malloc<T>(bytes:Int):cpp.Star<T>;
	public static function calloc<T>(bytes:Int):cpp.Star<T>;
	public static function realloc<T>(ioPtr:cpp.Star<T>, bytes:Int):cpp.Star<T>;
	public static function free<T>(ptr:cpp.Star<T>):Void;
	#end
}
