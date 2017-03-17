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
package java;

/**
	Platform-specific Java Library. Provides some platform-specific functions for the Java target,
	such as conversion from Haxe types to native types and vice-versa.
**/
//we cannot use the java package for custom classes, so we're redefining it as "haxe.java.Lib"
@:native('haxe.java.Lib') class Lib
{

	/**
		Returns a native array from the supplied Array. This native array is unsafe to be written on,
		as it may or may not be linked to the actual Array implementation.

		If `equalLengthRequired` is true, the result might be a copy of an array with the correct size.
	**/
	inline public static function nativeArray<T>(arr:Array<T>, equalLengthRequired:Bool):NativeArray<T>
	{
		var ret = new NativeArray(arr.length);
		for (i in 0...arr.length)
		{
			ret[i] = arr[i];
		}
		return ret;
	}

	/**
		Gets the native `java.lang.Class` from the supplied object. Will throw an exception in case of null being passed.
		[deprecated] - use `getNativeType` instead
	**/
	@:deprecated('The function `nativeType` is deprecated and will be removed in later versions. Please use `getNativeType` instead')
	inline public static function nativeType<T>(obj:T):java.lang.Class<T>
	{
		return untyped obj.getClass();
	}

	/**
		Gets the native `java.lang.Class` from the supplied object. Will throw an exception in case of null being passed.
	**/
	inline public static function getNativeType<T>(obj:T):java.lang.Class<T>
	{
		return untyped obj.getClass();
	}

	/**
		Returns a Class<> equivalent to the native java.lang.Class type.
	**/
	public static inline function fromNativeType<T>(t:java.lang.Class<T>):Class<T>
	{
		return untyped t;
	}

	/**
		Returns a java.lang.Class equivalent to the Haxe Class<> type.
	**/
	public static inline function toNativeType<T>(cl:Class<T>):java.lang.Class<T>
	{
		return untyped cl;
	}

	/**
		Returns a java.lang.Class equivalent to the Haxe Enum<> type.
	**/
	public static inline function toNativeEnum<T>(cl:Enum<T>):java.lang.Class<T>
	{
		return untyped cl;
	}

	/**
		Returns a Haxe Array of a native Array.
		Unless `copy` is true, it won't copy the contents of the native array,
		so unless any operation triggers an array resize, all changes made to the Haxe array will affect the native array argument.
	**/
	@:generic public static function array<T>(native:java.NativeArray<T>):Array<T>
	{
		return untyped Array.ofNative(native);
	}

	@:extern inline private static function doArray<T>(native:java.NativeArray<T>):Array<T>
	{
		var ret:NativeArray<Dynamic> = new NativeArray(native.length);
		for (i in 0...native.length)
		{
			ret[i] = native[i];
		}
		return untyped Array.ofNative(ret);
	}

	public static function array_Int(native:java.NativeArray<Int>):Array<Int>
	{
		return doArray(native);
	}

	public static function array_Float(native:java.NativeArray<Float>):Array<Float>
	{
		return doArray(native);
	}

	public static function array_Bool(native:java.NativeArray<Bool>):Array<Bool>
	{
		return doArray(native);
	}

	public static function array_java_Int8(native:java.NativeArray<java.StdTypes.Int8>):Array<java.StdTypes.Int8>
	{
		return doArray(native);
	}

	public static function array_java_Int16(native:java.NativeArray<java.StdTypes.Int16>):Array<java.StdTypes.Int16>
	{
		return doArray(native);
	}

	public static function array_java_Char16(native:java.NativeArray<java.StdTypes.Char16>):Array<java.StdTypes.Char16>
	{
		return doArray(native);
	}

	public static function array_Single(native:java.NativeArray<Single>):Array<Single>
	{
		return doArray(native);
	}

	public static function array_haxe_Int64(native:java.NativeArray<haxe.Int64>):Array<haxe.Int64>
	{
		return doArray(native);
	}

	/**
		Allocates a new Haxe Array with a predetermined size
	**/
	public static function arrayAlloc<T>(size:Int):Array<T>
	{
		return untyped Array.alloc(size);
	}

	/**
		Ensures that one thread does not enter a critical section of code while another thread
		is in the critical section. If another thread attempts to enter a locked code, it
		will wait, block, until the object is released.
		This is the equivalent to "synchronized" in java code.

		This method only exists at compile-time, so it can't be called via reflection.
	**/
	@:extern public static inline function lock<T>(obj:Dynamic, block:T):Void
	{
		untyped __lock__(obj, block);
	}
}
