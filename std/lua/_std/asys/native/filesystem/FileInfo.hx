package asys.native.filesystem;

import asys.native.system.SystemUser;
import asys.native.system.SystemGroup;

private typedef NativeInfo = lua.lib.luv.fs.FileSystem.Stat;

@:coreApi
abstract FileInfo(NativeInfo) from NativeInfo to NativeInfo {
	public var accessTime(get,never):Int;
	inline function get_accessTime():Int
		return this.atime.sec;

	public var modificationTime(get,never):Int;
	inline function get_modificationTime():Int
		return this.mtime.sec;

	public var creationTime(get,never):Int;
	inline function get_creationTime():Int
		return this.ctime.sec;

	public var deviceNumber(get,never):Int;
	inline function get_deviceNumber():Int
		return this.dev;

	public var group(get,never):SystemGroup;
	inline function get_group():SystemGroup
		return this.gid;

	public var user(get,never):SystemUser;
	inline function get_user():SystemUser
		return this.uid;

	public var inodeNumber(get,never):Int;
	inline function get_inodeNumber():Int
		return this.ino;

	public var mode(get,never):FileMode;
	inline function get_mode():FileMode
		return this.mode;

	public var links(get,never):Int;
	inline function get_links():Int
		return this.nlink;

	public var deviceType(get,never):Int;
	inline function get_deviceType():Int
		return this.rdev;

	public var size(get,never):Int;
	inline function get_size():Int
		return this.size;

	public var blockSize(get,never):Int;
	inline function get_blockSize():Int
		return this.blksize;

	public var blocks(get,never):Int;
	inline function get_blocks():Int
		return this.blocks;
}