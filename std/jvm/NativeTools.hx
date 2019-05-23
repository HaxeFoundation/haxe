package jvm;

extern class ObjectTools {
	static public inline function object<T>(t:T):java.lang.Object {
		return cast t;
	}
}

extern class NativeClassTools {
	static public inline function native<T>(c:Class<T>):java.lang.Class<T> {
		return cast c;
	}

	static public inline function haxe<T>(c:java.lang.Class<T>):Class<T> {
		return cast c;
	}

	static public inline function haxeEnum<T>(c:java.lang.Class<T>):std.Enum<T> {
		return cast c;
	}
}

extern class NativeEnumTools {
	static public inline function native<T>(e:std.Enum<Dynamic>):java.lang.Class<T> {
		return cast e;
	}
}