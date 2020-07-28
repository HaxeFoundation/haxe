package asys.native.filesystem;

import haxe.exceptions.NotImplementedException;

/**
	Data from system call to `stat`.

	TODO:
	- Decide on data type for time fields: `Date` means additional allocations;
		`Int` means "end-of-time" issue. Maybe `Float`?
	- Decide on `ino` type: theoretically it could be any big number. For example
		on Windows it could be a 64-bit unsigned integer. So may overflow.
	- Decide on `size` type: `Int` limits `size` to ~2GB.
**/
typedef FileStat = {
	/** Time of last access (Unix timestamp) */
	final atime:Int;
	/** Time of last modification (Unix timestamp) */
	final mtime:Int;
	/** Time of last inode change (Unix timestamp) */
	final ctime:Int;
	/** Device number */
	final dev:Int;
	/** Group id of owner */
	final gid:Int;
	/** User id of owner */
	final uid:Int;
	/** Inode number */
	final ino:Int;
	/** Inode protection mode */
	final mode:Int;
	/** Number of links */
	final nlink:Int;
	/** Device type, if inode device */
	final rdev:Int;
	/** Size in bytes */
	final size:Int;
	/** Block size of filesystem for IO operations */
	final blksize:Int;
	/** Number of 512-bytes blocks allocated */
	final blocks:Int;
};

/**
	Provides information about a file.
**/
abstract FileInfo(FileStat) from FileStat to FileStat {
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

	/** Time of last access (Unix timestamp) */
	public var accessTime(get,never):Int;
	inline function get_accessTime():Int
		return this.atime;

	/** Time of last modification (Unix timestamp) */
	public var modificationTime(get,never):Int;
	inline function get_modificationTime():Int
		return this.mtime;

	/** Time of last inode change (Unix timestamp) */
	public var creationTime(get,never):Int;
	inline function get_creationTime():Int
		return this.ctime;

	/** Device number */
	public var deviceNumber(get,never):Int;
	inline function get_deviceNumber():Int
		return this.dev;

	/** Group id of owner */
	public var groupId(get,never):Int;
	inline function get_groupId():Int
		return this.gid;

	/** User id of owner */
	public var userId(get,never):Int;
	inline function get_userId():Int
		return this.uid;

	/** Inode number */
	public var inodeNumber(get,never):Int;
	inline function get_inodeNumber():Int
		return this.ino;

	/** Inode protection mode */
	public var mode(get,never):Int;
	inline function get_mode():Int
		return this.mode;

	/** Number of links */
	public var links(get,never):Int;
	inline function get_links():Int
		return this.nlink;

	/** Device type, if inode device */
	public var deviceType(get,never):Int;
	inline function get_deviceType():Int
		return this.rdev;

	/** Size in bytes */
	public var size(get,never):Int;
	inline function get_size():Int
		return this.size;

	/** Block size of filesystem for IO operations */
	public var blockSize(get,never):Int;
	inline function get_blockSize():Int
		return this.blksize;

	/** Number of 512-bytes blocks allocated */
	public var blocks(get,never):Int;
	inline function get_blocks():Int
		return this.blocks;

	public inline function isBlockDevice():Bool
		return this.mode & S_IFMT == S_IFBLK;

	public inline function isCharacterDevice():Bool
		return this.mode & S_IFMT == S_IFCHR;

	public inline function isDirectory():Bool
		return this.mode & S_IFMT == S_IFDIR;

	/**
		TODO: Fifo? FiFo?
	**/
	public inline function isFIFO():Bool
		return this.mode & S_IFMT == S_IFIFO;

	public inline function isFile():Bool
		return this.mode & S_IFMT == S_IFREG;

	public inline function isSocket():Bool
		return this.mode & S_IFMT == S_IFSOCK;

	public inline function isSymbolicLink():Bool
		return this.mode & S_IFMT == S_IFLNK;
}