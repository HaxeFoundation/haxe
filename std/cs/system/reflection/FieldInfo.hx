package cs.system.reflection;

@:native("System.Reflection.FieldInfo")
extern class FieldInfo extends MemberInfo {
	var FieldType(default, never):cs.system.Type;
	var IsStatic(default, never):Bool;
	var IsPublic(default, never):Bool;
	var IsPrivate(default, never):Bool;
	var IsInitOnly(default, never):Bool;
	var IsLiteral(default, never):Bool;
	function GetValue(obj:Dynamic):Dynamic;
	function SetValue(obj:Dynamic, value:Dynamic):Void;
}
