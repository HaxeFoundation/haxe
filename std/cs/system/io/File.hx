package cs.system.io;

@:native('System.IO.File') extern class File 
{
	//not compatible with XNA
	//static function AppendAllText(path:String, contents:String):Void;
	static function Copy(source:String, dest:String):Void;
	
	@:overload(function(path:String, bufferSize:Int):FileStream {})
	static function Create(path:String):FileStream;
	
	static function Delete(path:String):Void;
	static function Exists(path:String):Bool;
	static function GetCreationTime(path:String):cs.system.DateTime;
	static function GetLastAccessTime(path:String):cs.system.DateTime;
	static function GetLastWriteTime(path:String):cs.system.DateTime;
	static function Move(source:String, dest:String):Void;
	
	
}