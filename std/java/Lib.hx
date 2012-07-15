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
	public static function toNativeReadOnlyArray<T>(arr:Array<T>, equalLengthRequired:Bool):NativeArray<T>
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
		Returns a System.Type equivalent to the Haxe Class<> type.
		
		Currently Haxe's Class<> is equivalent to System.Type, but this is an implementation detail.
		This may change in the future, so use this function whenever you need to perform such conversion.
	**/
	public static function toNativeType<T>(cl:Class<T>):java.lang.Class<T>
	{
		return untyped cl;
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
}