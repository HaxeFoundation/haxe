package rust;
import Array;
import rust.io.*;
/** Higher-level interfaces to libc.* functions and operating system services. */
@:native("std.os") extern class OS {
	public static var TMPBUF_SZ(default, null):Int;
	public static function args():Array<String>;
	public static function change_dir(p:Path):Bool;
	/** Copies a file from one location to another */
	public static function copy_file(from:Path, to:Path):Bool;
	public static function dll_filename(base:String):String;
	public static function errno():Int;
	public static function getcwd():Path;
	public static function getenv(n:String):Null<String>;
	/** Returns the path to the user's home directory, if known. */
	public static function homedir():Null<Path>;
	/** Get a string representing the platform-dependent last error */
	public static function last_os_error():String;
	/** Lists the contents of a directory */
	public static function list_dir(p:Path):Array<String>;
	/** Lists the contents of a directory */
	public static function list_dir_path(p:Path):Array<Path>;
	/** Convert a relative path to an absolute path */
	public static function make_absolute(p:Path):Path;
	/** Creates a directory at the specified path */
	public static function make_dir(p:Path, mode:Int):Bool;
	/** Indicates whether a path exists */
	public static function path_exists(p:Path):Bool;
	/** Indicates whether a path represents a directory */
	public static function path_is_dir(p:Path):Bool;
	public static function pipe():Pipe;
	public static function real_args():Array<String>;
	/** Removes a directory at the specified path */
	public static function remove_dir(p:Path):Bool;
	/** Deletes an existing file */
	public static function remove_file(p:Path):Bool;
	public static function self_exe_path():Path;
	public static function set_args(new_args:Array<String>):Void;
	/** Sets the process exit code */
	public static function set_exit_status(code:Int):Void;
	public static function setenv(n:String, v:String):Void;
	/** Returns the path to a temporary directory. */
	public static function tempdir():Path;
	public static function waitpid(pid:Int):Int;
	/** Recursively walk a directory structure */
	public static function walkdir(p:Path, f:Path->Bool):Void;
}

@:native("std.os.Pipe") extern class Pipe {
	public var input:Int;
	public var out:Int;
}
