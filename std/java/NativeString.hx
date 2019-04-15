package java;

import java.lang.CharSequence;

@:native("java.lang.String")
extern class NativeString {
	function charAt(index:Int):java.types.Char16;
	function codePointAt(index:Int):Int;
	function compareTo(s:String):Int;
	function concat(s:String):String;
	function endsWith(suffix:String):Bool;
	function equals(obj:Dynamic):Bool;
	function hashCode():Int;

	@:overload function indexOf(str:String):Int;
	@:overload function indexOf(str:String, fromIndex:Int):Int;

	function lastIndexOf(str:String, fromIndex:Int):Int;

	function replace(target:CharSequence, replacement:CharSequence):String;
	function startsWith(prefix:String):Bool;
	@:overload function substring(beginIndex:Int):String;
	@:overload function substring(beginIndex:Int, endIndex:Int):String;
}
