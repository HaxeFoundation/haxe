package asys.native.filesystem;

import haxe.exceptions.ArgumentException;
import haxe.exceptions.NotImplementedException;

private typedef NativeMode = Int;

/**
	File mode contains file type and permissions.
**/
@:coreApi
abstract FileMode(NativeMode) from NativeMode {
	/** file type bit mask */
	static inline var S_IFMT:Int = 61440;
	/** named pipe (fifo) */
	static inline var S_IFIFO:Int = 4096;
	/** character special */
	static inline var S_IFCHR:Int = 8192;
	/** directory */
	static inline var S_IFDIR:Int = 16384;
	/** block special */
	static inline var S_IFBLK:Int = 24576;
	/** regular */
	static inline var S_IFREG:Int = 32768;
	/** symbolic link */
	static inline var S_IFLNK:Int = 40960;
	/** socket */
	static inline var S_IFSOCK:Int = 49152;
	/** whiteout */
	static inline var S_IFWHT:Int = 57344;

	/**
		Check if all the permissions are set in this mode.
	**/
	public inline function has(permissions:FilePermissions):Bool {
		return this & (permissions:Int) == (permissions:Int);
	}

	public inline function isBlockDevice():Bool
		return this & S_IFMT == S_IFBLK;

	public inline function isCharacterDevice():Bool
		return this & S_IFMT == S_IFCHR;

	public inline function isDirectory():Bool
		return this & S_IFMT == S_IFDIR;

	/**
		TODO: Fifo? FiFo?
	**/
	public inline function isFIFO():Bool
		return this & S_IFMT == S_IFIFO;

	public inline function isFile():Bool
		return this & S_IFMT == S_IFREG;

	public inline function isSocket():Bool
		return this & S_IFMT == S_IFSOCK;

	public inline function isLink():Bool
		return this & S_IFMT == S_IFLNK;
}