package cs.system.reflection;

@:native("System.Reflection.ConstructorInfo")
extern class ConstructorInfo extends MethodBase {
	function Invoke(parameters:cs.NativeArray<Dynamic>):Dynamic;
}
