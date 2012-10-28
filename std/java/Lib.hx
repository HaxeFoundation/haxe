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
package java;

/**
	Platform-specific Java Library. Provides some platform-specific functions for the Java target,
	such as conversion from haxe types to native types and vice-versa.
**/
//we cannot use the java package for custom classes, so we're redefining it as "haxe.java.Lib"
@:native('haxe.java.Lib') class Lib 
{
	
	/**
		Returns a native array from the supplied Array. This native array is unsafe to be written on,
		as it may or may not be linked to the actual Array implementation.
		
		If equalLengthRequired is true, the result might be a copy of an array with the correct size.
	**/
	public static function nativeArray<T>(arr:Array<T>, equalLengthRequired:Bool):NativeArray<T>
	{
		var native:NativeArray<T> = untyped arr.__a;
		if (native.length == arr.length)
		{
			return native;
		} else {
			return null;
		}
	}
	
	/**
		Gets the native System.Type from the supplied object. Will throw an exception in case of null being passed.
	**/
	@:functionBody('
		return (java.lang.Class<T>) obj.getClass();
	')
	public static function nativeType<T>(obj:T):java.lang.Class<T>
	{
		return null;
	}
	
	/**
		Returns a Haxe Array of a native Array.
		It won't copy the contents of the native array, so unless any operation triggers an array resize,
		all changes made to the Haxe array will affect the native array argument.
	**/
	public static function array<T>(native:java.NativeArray<T>):Array<T>
	{
		return untyped Array.ofNative(native);
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
	@:extern public static inline function lock(obj:Dynamic, block:Dynamic):Void
	{
		untyped __lock__(obj, block);
	}
}