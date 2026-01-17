package cs.system;

@:native("System.Type")
extern class Type {
	var Name(default, never):String;
	var FullName(default, never):String;
	var Namespace(default, never):String;
	var BaseType(default, never):Type;
	var IsClass(default, never):Bool;
	var IsInterface(default, never):Bool;
	var IsEnum(default, never):Bool;
	var IsValueType(default, never):Bool;
	var IsArray(default, never):Bool;
	var IsGenericType(default, never):Bool;

	function GetFields():cs.NativeArray<cs.system.reflection.FieldInfo>;
	function GetMethods():cs.NativeArray<cs.system.reflection.MethodInfo>;
	function GetConstructors():cs.NativeArray<cs.system.reflection.ConstructorInfo>;
	function GetNestedTypes():cs.NativeArray<Type>;
	function GetField(name:String):cs.system.reflection.FieldInfo;
	function GetMethod(name:String):cs.system.reflection.MethodInfo;
	function GetProperty(name:String):cs.system.reflection.PropertyInfo;
	function GetInterfaces():cs.NativeArray<Type>;
	function GetGenericArguments():cs.NativeArray<Type>;
	function IsAssignableFrom(c:Type):Bool;
	function IsSubclassOf(c:Type):Bool;

	static function GetType(typeName:String):Type;
}
