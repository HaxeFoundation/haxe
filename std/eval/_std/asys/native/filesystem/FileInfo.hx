package asys.native.filesystem;

import haxe.exceptions.NotSupportedException;
import asys.native.system.SystemUser;
import asys.native.system.SystemGroup;
import eval.luv.File as LFile;

private typedef NativeInfo = eval.luv.File.FileStat;

@:coreApi
abstract FileInfo(NativeInfo) from NativeInfo to NativeInfo {
	public var accessTime(get,never):Int;
	inline function get_accessTime():Int
		return this.atim.sec.toInt();

	public var modificationTime(get,never):Int;
	inline function get_modificationTime():Int
		return this.mtim.sec.toInt();

	public var creationTime(get,never):Int;
	inline function get_creationTime():Int
		return this.ctim.sec.toInt();

	public var deviceNumber(get,never):Int;
	inline function get_deviceNumber():Int
		return this.dev.toInt();

	public var group(get,never):SystemGroup;
	inline function get_group():SystemGroup
		return this.gid.toInt();

	public var user(get,never):SystemUser;
	inline function get_user():SystemUser
		return this.uid.toInt();

	public var inodeNumber(get,never):Int;
	inline function get_inodeNumber():Int
		return this.ino.toInt();

	public var mode(get,never):FileMode;
	inline function get_mode():FileMode
		return this.mode;

	public var links(get,never):Int;
	inline function get_links():Int
		return this.nlink.toInt();

	public var deviceType(get,never):Int;
	inline function get_deviceType():Int
		return this.rdev.toInt();

	public var size(get,never):Int;
	inline function get_size():Int
		return this.size.toInt();

	public var blockSize(get,never):Int;
	inline function get_blockSize():Int
		return this.blksize.toInt();

	public var blocks(get,never):Int;
	inline function get_blocks():Int
		return this.blocks.toInt();
}