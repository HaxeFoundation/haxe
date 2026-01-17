package cs.system.reflection;

@:native("System.Reflection.MethodBase")
extern class MethodBase {
	var Name(default, never):String;
	var DeclaringType(default, never):cs.system.Type;
	var IsStatic(default, never):Bool;
	var IsPublic(default, never):Bool;
	var IsPrivate(default, never):Bool;
	function GetParameters():cs.NativeArray<ParameterInfo>;
}

@:native("System.Reflection.ParameterInfo")
extern class ParameterInfo {
	var Name(default, never):String;
	var ParameterType(default, never):cs.system.Type;
	var Position(default, never):Int;
}
