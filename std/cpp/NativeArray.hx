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

extern class NativeArray {
	#if cppia
	static inline function create<T>(length:Int):Array<T> {
		var result = new Array<T>();
		NativeArray.setSize(result, length);
		return result;
	}
	#else
	@:native("_hx_create_array_length")
	static function create<T>(length:Int):Array<T>;
	#end

	static inline function blit<T>(ioDestArray:Array<T>, inDestElement:Int, inSourceArray:Array<T>, inSourceElement:Int, inElementCount:Int):Void {
		untyped ioDestArray.blit(inDestElement, inSourceArray, inSourceElement, inElementCount);
	};

	static inline function getBase(inArray:Array<Dynamic>):ArrayBase {
		return untyped inArray;
	}

	@:nativeStaticExtension
	static function reserve<T>(inArray:Array<T>, inElements:Int):Void;

	@:nativeStaticExtension
	static function capacity<T>(inArray:Array<T>):Int;

	@:nativeStaticExtension
	static function getElementSize<T>(inArray:Array<T>):Int;

	static inline function address<T>(inArray:Array<T>, inIndex:Int):Pointer<T> {
		return Pointer.arrayElem(inArray, inIndex);
	}

	@:nativeStaticExtension
	static function setData<T>(inArray:Array<T>, inData:Pointer<T>, inElementCount:Int):Void;

	@:nativeStaticExtension
	static function setUnmanagedData<T>(inArray:Array<T>, inData:ConstPointer<T>, inElementCount:Int):Void;

	@:nativeStaticExtension
	static function zero<T>(ioDestArray:Array<T>, ?inFirst:Int, ?inElements:Int):Void;

	@:nativeStaticExtension
	static function removeAt<T>(ioDestArray:Array<T>, inIndex:Int):Void;

	@:nativeStaticExtension
	static function memcmp<T>(inArrayA:Array<T>, inArrayB:Array<T>):Int;

	@:native("_hx_reslove_virtual_array")
	static function resolveVirtualArray(inArray:Array<Dynamic>):Dynamic;

	#if cppia
	static inline function unsafeGet<T>(inDestArray:Array<T>, inIndex:Int):T {
		return untyped inDestArray.__unsafe_get(inIndex);
	}

	static inline function unsafeSet<T>(ioDestArray:Array<T>, inIndex:Int, inValue:T):T {
		return untyped ioDestArray.__unsafe_set(inIndex, inValue);
	}

	static inline function setSize<T>(ioArray:Array<T>, inSize:Int):Array<T> {
		return untyped ioArray.__SetSizeExact(inSize);
	}
	#else
	@:native("_hx_array_unsafe_get")
	static function unsafeGet<T>(inDestArray:Array<T>, inIndex:Int):T;

	@:native("_hx_array_unsafe_set")
	static inline function unsafeSet<T>(ioDestArray:Array<T>, inIndex:Int, inValue:T):T {
		return untyped ioDestArray.__unsafe_set(inIndex, inValue);
	}

	@:native("_hx_array_set_size_exact")
	static function setSize<T>(ioArray:Array<T>, inSize:Int):Array<T>;
	#end
}
