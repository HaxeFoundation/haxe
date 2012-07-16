package cs.system.io;
import cs.NativeArray;
import cs.NativeArray;

@:native('System.IO.Directory') extern class Directory 
{
	static function CreateDirectory(path:String):Void;
	static function Delete(path:String):Void;
	static function Exists(path:String):Bool;
	static function GetCurrentDirectory():String;
	static function GetDirectories(path:String):NativeArray<String>;
	static function GetDirectoryRoot(path:String):String;
	static function GetFiles(path:String):NativeArray<String>;
	static function GetFileSystemEntries(path:String):NativeArray<String>;
	static function GetCreationTime(path:String):cs.system.DateTime;
	static function GetLastAccessTime(path:String):cs.system.DateTime;
	static function GetLastWriteTime(path:String):cs.system.DateTime;
	static function Move(path:String, newpath:String):Void;
	static function SetCurrentDirectory(path:String):Void;
}