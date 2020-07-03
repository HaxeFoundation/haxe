package asys.native.filesystem;

import haxe.exceptions.NotImplementedException;

/**
	Data from system call to `stat`.

	TODO:
	- Decide on data type for time fields: `Date` means additional allocations;
		`Int` means "end-of-time" issue. Maybe `Float`?
	- Decide on `ino` type: theoretically it could be any big number. `Int` may
		not fit it in future.
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
	/** Time of last access (Unix timestamp) */
	var accessTime(get,never):Int;
	inline function get_accessTime() return this.atime;

	/** Time of last modification (Unix timestamp) */
	var modificationTime(get,never):Int;
	inline function get_modificationTime() return this.mtime;

	/** Time of last inode change (Unix timestamp) */
	var creationTime(get,never):Int;
	inline function get_creationTime() return this.ctime;

	/** Device number */
	var deviceNumber(get,never):Int;
	inline function get_deviceNumber() return this.dev;

	/** Group id of owner */
	var groupId(get,never):Int;
	inline function get_groupId() return this.gid;

	/** User id of owner */
	var userId(get,never):Int;
	inline function get_userId() return this.uid;

	/** Inode number */
	var inodeNumber(get,never):Int;
	inline function get_inodeNumber() return this.ino;

	/** Inode protection mode */
	var mode(get,never):Int;
	inline function get_mode() return this.mode;

	/** Number of links */
	var links(get,never):Int;
	inline function get_links() return this.nlink;

	/** Device type, if inode device */
	var deviceType(get,never):Int;
	inline function get_deviceType() return this.rdev;

	/** Size in bytes */
	var size(get,never):Int;
	inline function get_size() return this.size;

	/** Block size of filesystem for IO operations */
	var blockSize(get,never):Int;
	inline function get_blockSize() return this.blksize;

	/** Number of 512-bytes blocks allocated */
	var blocks(get,never):Int;
	inline function get_blocks() return this.blocks;

	public function isBlockDevice():Bool {
		throw new NotImplementedException();
	}

	public function isCharacterDevice():Bool {
		throw new NotImplementedException();
	}

	public function isDirectory():Bool {
		throw new NotImplementedException();
	}

	/**
		TODO: Fifo? FiFo?
	**/
	public function isFIFO():Bool {
		throw new NotImplementedException();
	}

	public function isFile():Bool {
		throw new NotImplementedException();
	}

	public function isSocket():Bool {
		throw new NotImplementedException();
	}

	public function isSymbolicLink():Bool {
		throw new NotImplementedException();
	}
}