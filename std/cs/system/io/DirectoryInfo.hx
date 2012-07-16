package cs.system.io;
import haxe.Int64;

@:final @:native('System.IO.DirectoryInfo') extern class DirectoryInfo 
{
	var Attributes(default, null):FileAttributes;
	var CreationTime(default, null):cs.system.DateTime;
	var Exists(default, null):Bool;
	var Extension(default, null):String;
	var FullName(default, null):String;
	var LastAccessTime(default, null):cs.system.DateTime;
	var LastWriteTime(default, null):cs.system.DateTime;
	var Name(default, null):String;
	var Parent(default, null):DirectoryInfo;
	var Root(default, null):DirectoryInfo;
	
	function new(path:String):Void;
}