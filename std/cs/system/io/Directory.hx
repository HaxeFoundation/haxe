package cs.system.io;

@:native("System.IO.Directory")
extern class Directory {
	static function Exists(path:String):Bool;
	static function Move(sourceDirName:String, destDirName:String):Void;
	static function CreateDirectory(path:String):DirectoryInfo;
	static function Delete(path:String):Void;
	static function GetFileSystemEntries(path:String):cs.NativeArray<String>;
	static function GetCurrentDirectory():String;
	static function SetCurrentDirectory(path:String):Void;
}
