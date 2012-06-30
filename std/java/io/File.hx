package java.io;
import haxe.Int64;

extern class File 
{
	@:overload(function(prefix:String, suffix:String, dir:File):File { })
	static function createTempFile(prefix:String, suffix:String):File;
	static function listRoots():java.NativeArray<File>;
	
	function new(pathName:String):Void;
	
	function canRead():Bool;
	function canWrite():Bool;
	function createNewFile():Bool;
	function delete():Bool;
	function deleteOnExit():Void;
	function exists():Bool;
	
	function getAbsoluteFile():File;
	function getAbsolutePath():String;
	function getCanonicalFile():File;
	function getCanonicalPath():String;
	
	function getName():String;
	function getPath():String;
	
	function isAbsolute():Bool;
	function isDirectory():Bool;
	function isFile():Bool;
	function isHidden():Bool;
	function lastModified():Int64;
	function length():Int64;
	
	function list():java.NativeArray<String>;
	function listFiles():java.NativeArray<String>;
	
	function mkdir():Bool;
	function mkdirs():Bool;
	function renameTo(dest:File):Bool;
	function setLastModified(time:Int64):Bool;
	function setReadOnly():Bool;
}