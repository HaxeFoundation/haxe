package cs.system.io;

@:native("System.IO.DirectoryInfo")
extern class DirectoryInfo {
	function new(path:String):Void;
	var LastAccessTime(default, never):cs.system.DateTime;
	var LastWriteTime(default, never):cs.system.DateTime;
	var CreationTime(default, never):cs.system.DateTime;
}
