package cs.system.io;
import haxe.Int64;

@:final @:native('System.IO.FileInfo') extern class FileInfo 
{
	var Attributes(default, null):FileAttributes;
	var CreationTime(default, null):cs.system.DateTime;
	var Directory(default, null):DirectoryInfo;
	var DirectoryName(default, null):String;
	var Exists(default, null):Bool;
	var Extension(default, null):String;
	var FullName(default, null):String;
	var LastAccessTime(default, null):cs.system.DateTime;
	var LastWriteTime(default, null):cs.system.DateTime;
	var Length(default, null):Int64;
	var Name(default, null):String;
	
	function new(filename:String):Void;
}