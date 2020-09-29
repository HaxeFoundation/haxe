package asys.native.filesystem;

import haxe.exceptions.NotSupportedException;
import asys.native.system.SystemUser;
import asys.native.system.SystemGroup;
import eval.Unix;

private typedef NativeInfo = Stats;

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

	public var permissions(get,never):FilePermissions;
	inline function get_permissions():FilePermissions
		return this.st_perm;

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
		throw NotSupportedException.field();

	public var blocks(get,never):Int;
	inline function get_blocks():Int
		throw NotSupportedException.field();

	public inline function isBlockDevice():Bool
		return this.st_kind == S_BLK;

	public inline function isCharacterDevice():Bool
		return this.st_kind == S_CHR;

	public inline function isDirectory():Bool
		return this.st_kind == S_DIR;

	public inline function isFIFO():Bool
		return this.st_kind == S_FIFO;

	public inline function isFile():Bool
		return this.st_kind == S_REG;

	public inline function isSocket():Bool
		return this.st_kind == S_SOCK;

	public inline function isSymbolicLink():Bool
		return this.st_kind == S_LNK;
}