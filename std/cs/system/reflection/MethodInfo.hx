package cs.system.reflection;

@:native("System.Reflection.MethodInfo")
extern class MethodInfo extends MethodBase {
	var ReturnType(default, never):cs.system.Type;
	function Invoke(obj:Dynamic, parameters:cs.NativeArray<Dynamic>):Dynamic;
}
