package cs.system.reflection;
import cs.NativeArray;

@:native('System.Reflection.ConstructorInfo') extern class ConstructorInfo extends MethodBase
{
	@:overload(function(args:NativeArray<Dynamic>):Dynamic {})
	override function Invoke(obj:Dynamic, args:NativeArray<Dynamic>):Dynamic;
}