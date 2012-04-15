package cs;
import cs.native.Type;

/**
 * ...
 * @author waneck
 */

class Lib 
{

	public static function toNativeReadOnlyArray<T>(arr:Array<T>, equalLengthRequired:Bool):NativeArray<T>
	{
		var native:NativeArray<T> = untyped arr.__a;
		if (native.Length == arr.length)
		{
			return native;
		} else {
			return null;
		}
	}
	
	@:functionBody('
			throw new Haxe.Lang.HaxeException("This function cannot be accessed at runtime");
	')
	public static inline function as<T>(obj:Dynamic, cl:Class<T>):T
	{
		return untyped __as__(obj, cl);
	}
	
	public static function toNativeType(cl:Class<Dynamic>):Type
	{
		return untyped cl.nativeType();
	}
	
	@:functionBody('
			return obj.GetType();
	')
	public static function getNativeType(obj:Dynamic):Type
	{
		return null;
	}
	
	@:functionBody('System.Console.ReadLine();')
	public static function wait():Void
	{
		
	}
}