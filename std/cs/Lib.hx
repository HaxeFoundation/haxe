package cs;
import system.Type;

/**
	Platform-specific C# Library. Provides some platform-specific functions for the C# target,
	such as conversion from haxe types to native types and vice-versa.
**/
class Lib 
{
	@:keep private static var decimalSeparator:String;
	
	/**
		Changes the current culture settings to allow a consistent cross-target behavior.
		Currently the only change made is in regard to the decimal separator, which is always set to "."
	**/
	@:functionBody('
			System.Globalization.CultureInfo ci = new System.Globalization.CultureInfo(System.Threading.Thread.CurrentThread.CurrentCulture.Name, true);
			decimalSeparator = ci.NumberFormat.NumberDecimalSeparator;
            ci.NumberFormat.NumberDecimalSeparator = ".";
            System.Threading.Thread.CurrentThread.CurrentCulture = ci;
	')
	@:keep public static function applyCultureChanges():Void
	{
		
	}
	
	/**
		Reverts the culture changes to the default settings.
	**/
	@:functionBody('
		System.Globalization.CultureInfo ci = new System.Globalization.CultureInfo(System.Threading.Thread.CurrentThread.CurrentCulture.Name, true);
		System.Threading.Thread.CurrentThread.CurrentCulture = ci;
	')
	public static function revertDefaultCulture():Void
	{
		
	}
	
	/**
		Returns a native array from the supplied Array. This native array is unsafe to be written on,
		as it may or may not be linked to the actual Array implementation.
		
		If equalLengthRequired is true, the result might be a copy of an array with the correct size.
	**/
	public static function toNativeReadOnlyArray<T>(arr:Array<T>, equalLengthRequired:Bool):NativeArray<T>
	{
		var native:NativeArray<T> = untyped arr.__a;
		var len = arr.length;
		if (!equalLengthRequired || native.Length == len)
		{
			return native;
		} else {
			var ret = new NativeArray<T>(len);
			system.Array.Copy(native, 0, ret, 0, len);
			return ret;
		}
	}
	
	/**
		Provides support for the "as" keyword in C#.
		If the object is not of the supplied type "T", it will return null instead of rasing an exception.
		
		This function will not work with Value Types (such as Int, Float, Bool...)
	**/
	@:functionBody('
			throw new haxe.lang.HaxeException("This function cannot be accessed at runtime");
	')
	@:extern public static inline function as<T>(obj:Dynamic, cl:Class<T>):T
	{
		return untyped __as__(obj, cl);
	}
	
	/**
		Returns a Class<> equivalent to the native System.Type type.
		
		Currently Haxe's Class<> is equivalent to System.Type, but this is an implementation detail.
		This may change in the future, so use this function whenever you need to perform such conversion.
	**/
	public static inline function fromNativeType(t:system.Type):Class<Dynamic>
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
		Gets the native System.Type from the supplied object. Will throw an exception in case of null being passed.
	**/
	public static function nativeType(obj:Dynamic):Type
	{
		return untyped obj.GetType();
	}
	
	/**
		Returns a Haxe Array of a native Array.
		It won't copy the contents of the native array, so unless any operation triggers an array resize,
		all changes made to the Haxe array will affect the native array argument.
	**/
	public static function array<T>(native:cs.NativeArray<T>):Array<T>
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