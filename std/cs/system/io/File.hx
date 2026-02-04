package cs.system.io;

@:native("System.IO.File")
extern class File {
	static function Exists(path:String):Bool;
	static function Delete(path:String):Void;
	static function Move(sourceFileName:String, destFileName:String):Void;
}
