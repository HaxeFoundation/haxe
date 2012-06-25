package cs;
import system.Type;

/**
	Platform-specific C# Library. Provides some platform-specific functions for the C# target,
	such as conversion from haxe types to native types.
**/

class Lib 
{
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
		
		This function will not work with Value Types (such as Int, Float, Bool...) conversion
	**/
	@:functionBody('
			throw new haxe.lang.HaxeException("This function cannot be accessed at runtime");
	')
	public static inline function as<T>(obj:Dynamic, cl:Class<T>):T
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
	@:functionBody('
			return obj.GetType();
	')
	public static function getNativeType(obj:Dynamic):Type
	{
		return untyped obj.GetType();
	}
}