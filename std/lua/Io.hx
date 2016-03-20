package lua;
import haxe.extern.Rest;

@:native("io")
extern class Io {
	public static function close(?file : FileHandle) : Void;
	public static function flush() : Void;

	@:overload(   function      (file : String)     : Void {})
	public static function input(file : FileHandle) : Void;

	public static function lines(?file : String) : NativeIterator<String>; 

	public static function open (filename : String, ?mode : String) : FileHandle;
	public static function popen(command : String, ?mode : String) : FileHandle;

	@:overload(   function     (?count    : Int)    : String {})
	public static function read(?filename : String) : String;

	public static function write(v : Rest<String>) : Void;

	// TODO
	// public static function write(v : Dynamic)  
// public static function output
// public static function popen
// public static function stderr
// public static function stdin
// public static function stdout
// public static function tmpfile
// public static function type
// public static function write

}

