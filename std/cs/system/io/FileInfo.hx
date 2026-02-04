package cs.system.io;

@:native("System.IO.FileInfo")
extern class FileInfo {
	function new(fileName:String):Void;
	var Length(default, never):haxe.Int64;
	var LastAccessTime(default, never):cs.system.DateTime;
	var LastWriteTime(default, never):cs.system.DateTime;
	var CreationTime(default, never):cs.system.DateTime;
	var FullName(default, never):String;
}
