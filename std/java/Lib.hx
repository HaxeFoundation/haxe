package java;
import java.lang.Class;

/**
 * ...
 * @author waneck
 */

//we cannot use the java package for custom classes, so we're redefining it as "haxe.java.Lib"
@:native('haxe.java.Lib') class Lib 
{

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
	
	public static function toNativeType<T>(cl:Class<T>):java.lang.Class<T>
	{
		return untyped cl.nativeType();
	}
	
	@:functionBody('
		return (java.lang.Class<T>) obj.getClass();
	')
	public static function getNativeType<T>(obj:T):java.lang.Class<T>
	{
		return null;
	}
}