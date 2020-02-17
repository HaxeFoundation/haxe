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

package neko;

class NativeArray<T> implements ArrayAccess<T> {
	public static inline function alloc<T>(length:Int):NativeArray<T> {
		return untyped __dollar__amake(length);
	}

	public static inline function blit<T>(dst:NativeArray<T>, dstPos:Int, src:NativeArray<T>, srcPos:Int, length:Int) {
		untyped __dollar__ablit(dst, dstPos, src, srcPos, length);
	}

	public static inline function ofArrayCopy<T>(a:Array<T>):NativeArray<T> {
		return untyped a.__neko();
	}

	public static inline function ofArrayRef<T>(a:Array<T>):NativeArray<T> {
		return untyped a.__a;
	}

	public static inline function sub<T>(a:NativeArray<T>, pos:Int, len:Int):NativeArray<T> {
		return untyped __dollar__asub(a, pos, len);
	}

	public static inline function toArray<T>(a:NativeArray<T>):Array<T> {
		return untyped Array.new1(a, __dollar__asize(a));
	}

	public static inline function length(a:NativeArray<Dynamic>):Int {
		return untyped __dollar__asize(a);
	}
}
