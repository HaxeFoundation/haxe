package cs.system.reflection;

@:native("System.Reflection.MemberInfo")
extern class MemberInfo {
	var Name(default, never):String;
	var DeclaringType(default, never):cs.system.Type;
}
