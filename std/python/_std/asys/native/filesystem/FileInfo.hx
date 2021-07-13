package asys.native.filesystem;

import haxe.exceptions.NotImplementedException;
import asys.native.system.SystemUser;
import asys.native.system.SystemGroup;

private typedef NativeInfo = python.lib.Os.Stat;

@:coreApi
abstract FileInfo(NativeInfo) from NativeInfo to NativeInfo {
	public var accessTime(get,never):Int;
	inline function get_accessTime():Int
		return this.st_atime;

	public var modificationTime(get,never):Int;
	inline function get_modificationTime():Int
		return this.st_mtime;

	public var creationTime(get,never):Int;
	inline function get_creationTime():Int
		return this.st_ctime;

	public var deviceNumber(get,never):Int;
	inline function get_deviceNumber():Int
		return this.st_dev;

	public var group(get,never):SystemGroup;
	inline function get_group():SystemGroup
		return this.st_gid;

	public var user(get,never):SystemUser;
	inline function get_user():SystemUser
		return this.st_uid;

	public var inodeNumber(get,never):Int;
	inline function get_inodeNumber():Int
		return this.st_ino;

	public var mode(get,never):FileMode;
	inline function get_mode():FileMode
		return this.st_mode;

	public var links(get,never):Int;
	inline function get_links():Int
		return this.st_nlink;

	public var deviceType(get,never):Int;
	inline function get_deviceType():Int
		return this.st_rdev;

	public var size(get,never):Int;
	inline function get_size():Int
		return this.st_size;

	public var blockSize(get,never):Int;
	inline function get_blockSize():Int
		return this.st_blksize;

	public var blocks(get,never):Int;
	inline function get_blocks():Int
		return this.st_blocks;
}