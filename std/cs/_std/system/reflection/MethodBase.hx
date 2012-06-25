package system.reflection;
import cs.NativeArray;

@:native('System.Reflection.MethodBase') extern class MethodBase 
{
	var Name(default, null):String;
	function GetParameters():NativeArray<ParameterInfo>;
	function Invoke(obj:Dynamic, args:NativeArray<Dynamic>):Dynamic;
}