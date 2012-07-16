package cs.system.reflection;
import cs.NativeArray;

@:native('System.Reflection.MethodBase') extern class MethodBase 
{
	var Name(default, null):String;
	var ContainsGenericParameters(default, null):Bool;
	function GetParameters():NativeArray<ParameterInfo>;
	function GetGenericArguments():NativeArray<cs.system.Type>;
	@:overload(function():Dynamic {})
	function Invoke(obj:Dynamic, args:NativeArray<Dynamic>):Dynamic;
}