package cs.system.reflection;

@:native("System.Reflection.PropertyInfo")
extern class PropertyInfo extends MemberInfo {
	var PropertyType(default, never):cs.system.Type;
	var CanRead(default, never):Bool;
	var CanWrite(default, never):Bool;
	function GetValue(obj:Dynamic):Dynamic;
	function SetValue(obj:Dynamic, value:Dynamic):Void;
	function GetGetMethod():MethodInfo;
	function GetSetMethod():MethodInfo;
}
