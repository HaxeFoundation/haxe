package java.lang;

extern class Class<T>
{
	function isAssignableFrom(cls:Class<Dynamic>):Bool;
	function getName():String;
	function getResourceAsStream(name:String):java.io.InputStream;
}