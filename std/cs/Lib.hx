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
package cs;
import cs.system.Type;

/**
	Platform-specific C# Library. Provides some platform-specific functions for the C# target,
	such as conversion from haxe types to native types and vice-versa.
**/
class Lib
{
	private static var decimalSeparator:String;

	/**
		Changes the current culture settings to allow a consistent cross-target behavior.
		Currently the only change made is in regard to the decimal separator, which is always set to "."
	**/
	public static function applyCultureChanges():Void
	{
		var ci = new cs.system.globalization.CultureInfo(cs.system.threading.Thread.CurrentThread.CurrentCulture.Name, true);
		decimalSeparator = ci.NumberFormat.NumberDecimalSeparator;
		ci.NumberFormat.NumberDecimalSeparator = ".";
		cs.system.threading.Thread.CurrentThread.CurrentCulture = ci;
	}

	/**
		Reverts the culture changes to the default settings.
	**/
	public static function revertDefaultCulture():Void
	{
		var ci = new cs.system.globalization.CultureInfo(cs.system.threading.Thread.CurrentThread.CurrentCulture.Name, true);
		cs.system.threading.Thread.CurrentThread.CurrentCulture = ci;
	}

	/**
		Returns a native array from the supplied Array. This native array is unsafe to be written on,
		as it may or may not be linked to the actual Array implementation.

		If equalLengthRequired is true, the result might be a copy of an array with the correct size.
	**/
	@:extern inline public static function nativeArray<T>(arr:Array<T>, equalLengthRequired:Bool):NativeArray<T>
	{
		var ret = new cs.NativeArray(arr.length);
#if erase_generics
		for (i in 0...arr.length)
			ret[i] = arr[i];
#else
		p_nativeArray(arr,ret);
#end
		return ret;
	}

#if !erase_generics
	static function p_nativeArray<T>(arr:Array<T>, ret:cs.system.Array):Void
	{
		var native:NativeArray<T> = untyped arr.__a;
		var len = arr.length;
		cs.system.Array.Copy(native, 0, ret, 0, len);
	}
#end

	/**
		Provides support for the "as" keyword in C#.
		If the object is not of the supplied type "T", it will return null instead of rasing an exception.

		This function will not work with Value Types (such as Int, Float, Bool...)
	**/
	@:pure
	@:extern public static inline function as<T>(obj:Dynamic, cl:Class<T>):T
	{
		return untyped __as__(obj);
	}

	/**
		Returns a Class<> equivalent to the native System.Type type.

		Currently Haxe's Class<> is equivalent to System.Type, but this is an implementation detail.
		This may change in the future, so use this function whenever you need to perform such conversion.
	**/
	public static inline function fromNativeType(t:cs.system.Type):Class<Dynamic>
	{
		return untyped t;
	}

	/**
		Returns a System.Type equivalent to the Haxe Class<> type.

		Currently Haxe's Class<> is equivalent to System.Type, but this is an implementation detail.
		This may change in the future, so use this function whenever you need to perform such conversion.
	**/
	public static inline function toNativeType(cl:Class<Dynamic>):Type
	{
		return untyped cl;
	}

	/**
		Returns a System.Type equivalent to the Haxe Enum<> type.
	**/
	public static inline function toNativeEnum(cl:Enum<Dynamic>):Type
	{
		return untyped cl;
	}

	/**
		Gets the native System.Type from the supplied object. Will throw an exception in case of null being passed.
		[deprecated] - use `getNativeType` instead
	**/
	@:deprecated('The function `nativeType` is deprecated and will be removed in later versions. Please use `getNativeType` instead')
	public static inline function nativeType(obj:Dynamic):Type
	{
		return untyped obj.GetType();
	}

	/**
		Gets the native System.Type from the supplied object. Will throw an exception in case of null being passed.
	**/
	public static inline function getNativeType(obj:Dynamic):Type
	{
		return untyped obj.GetType();
	}

#if erase_generics
	inline private static function mkDynamic<T>(native:NativeArray<T>):NativeArray<Dynamic>
	{
		var ret = new cs.NativeArray<Dynamic>(native.Length);
		for (i in 0...native.Length)
			ret[i] = native[i];
		return ret;
	}
#end

	/**
		Returns a Haxe Array of a native Array.
		Unless `erase_generics` is defined, it won't copy the contents of the native array,
		so unless any operation triggers an array resize, all changes made to the Haxe array
		will affect the native array argument.
	**/
	inline public static function array<T>(native:cs.NativeArray<T>):Array<T>
	{
#if erase_generics
		var dyn:NativeArray<Dynamic> = mkDynamic(native);
		return @:privateAccess Array.ofNative(dyn);
#else
		return @:privateAccess Array.ofNative(native);
#end
	}

	/**
		Allocates a new Haxe Array with a predetermined size
	**/
	inline public static function arrayAlloc<T>(size:Int):Array<T>
	{
		return @:privateAccess Array.alloc(size);
	}

	/**
		Rethrow an exception. This is useful when manually filtering an exception in order
		to keep the previous exception stack.
	**/
	@:extern inline public static function rethrow(e:Dynamic):Void
	{
		untyped __rethrow__();
	}

	/**
		Creates a "checked" block, which throws exceptions for overflows.

		Usage:
			cs.Lib.checked({
				var x = 1000;
				while(true)
				{
					x *= x;
				}
			});
		This method only exists at compile-time, so it can't be called via reflection.
	**/
	@:extern public static inline function checked<V>(block:V):Void
	{
		untyped __checked__(block);
	}

	/**
		Ensures that one thread does not enter a critical section of code while another thread
		is in the critical section. If another thread attempts to enter a locked code, it
		will wait, block, until the object is released.

		This method only exists at compile-time, so it can't be called via reflection.
	**/
	@:extern public static inline function lock<O,V>(obj:O, block:V):Void
	{
		untyped __lock__(obj, block);
	}

	//Unsafe code manipulation
	#if unsafe
	/**
		Marks its parameters as fixed objects inside the defined block.
		The first variable declarations that use cs.Lib.pointerOfArray() will be the fixed definitions.
		Usage:
			cs.Lib.fixed({
				var obj1 = cs.Lib.pointerOfArray(someArray);
				var obj2 = cs.Lib.pointerOfArray(someArray2);
				var obj3 = cs.Lib.pointerOfArray(someArray3);
				//from now on, obj1, obj2 and obj3 are fixed
				//we cannot change obj1, obj2 or obj3 variables like this:
				//obj1++;
			});

		This method only exists at compile-time, so it can't be called via reflection.
	**/
	@:extern public static inline function fixed<V>(block:V):Void
	{
		untyped __fixed__(block);
	}

	/**
		Marks the contained block as an unsafe block, meaning that it can contain unsafe code.
		Usage:
			cs.Lib.unsafe({
				//unsafe code is allowed inside here
			});

		This method only exists at compile-time, so it can't be called via reflection.
	**/
	@:extern public static inline function unsafe<V>(block:V):Void
	{
		untyped __unsafe__(block);
	}

	/**
		Gets the pointer to the address of current local. Equivalent to the "&" operator in C#
		Usage:
			var x:Int = 0;
			cs.Lib.unsafe({
				var addr = cs.Lib.addressOf(x);
				x[0] = 42;
			});
			trace(x); //42

		This method only exists at compile-time, so it can't be called via reflection.
		Warning: This method will only work if a local variable is passed as an argument.
	**/
	@:extern public static inline function addressOf<T>(variable:T):cs.Pointer<T>
	{
		return untyped __addressOf__(variable);
	}

	/**
		Gets the value of the pointer address.
		Usage:
			var x:Int = 0;
			cs.Lib.unsafe({
				var addr = cs.Lib.addressOf(x);
				trace(cs.Lib.valueOf(addr)); //0
				addr[0] = 42;
				trace(cs.Lib.valueOf(addr)); //42
			});
			trace(x); //42

		This method only exists at compile-time, so it can't be called via reflection.
	**/
	@:extern public static inline function valueOf<T>(pointer:cs.Pointer<T>):T
	{
		return untyped __valueOf__(pointer);
	}

	/**
		Transforms a managed native array into a Pointer. Must be inside a fixed statement
		Usage:
			var x:cs.NativeArray<Int> = new cs.NativeArray(1);
			cs.Lib.unsafe({
				cs.Lib.fixed({
					var addr = cs.Lib.pointerOfArray(x);
					trace(cs.Lib.valueOf(addr)); //0
					addr[0] = 42;
					trace(cs.Lib.valueOf(addr)); //42
				});
			});
			trace(x[0]); //42

		This method only exists at compile-time, so it can't be called via reflection.
	**/
	@:extern public static inline function pointerOfArray<T>(array:cs.NativeArray<T>):cs.Pointer<T>
	{
		return untyped __ptr__(array);
	}

	/**
		Returns the byte size of the given struct. Only works with structs and basic types.
	**/
	@:extern public static inline function sizeof(struct:Class<Dynamic>):Int
	{
		return untyped __sizeof__(struct);
	}
	#end
}
