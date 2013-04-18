package rust.io;
import rust.*;
import rust.type.*;
@:native("path.Path") extern class Path implements Clone implements ToStr {
	public var is_absolute(default, null):Bool;
	public var components(default, null):Array<String>;
	public function exists():Bool;
	public function getSize():Null<haxe.Int64>;
	public function getMode():Null<Int>;
	public static function from_str(s:String):Path;
	public function dirname():String;
	public function filename():Null<String>;
	public function filestem():Null<String>;
	public function filetype():Null<String>;
	public function with_dirname(d:String):Path;
	public function with_filename(d:String):Path;
	public function with_filestem(d:String):Path;
	public function with_filetype(d:String):Path;
	public function dir_path():Path;
	public function file_path():Path;
	public function clone():Path;
	public function to_str():String;
}