package asys.native.filesystem;

import asys.native.system.SystemUser;
import asys.native.system.SystemGroup;
import java.util.concurrent.TimeUnit;
import haxe.exceptions.NotSupportedException;

using haxe.Int64;

private typedef NativeInfo = java.nio.file.attribute.PosixFileAttributes;

@:coreApi
abstract FileInfo(NativeInfo) from NativeInfo to NativeInfo {
	public var accessTime(get,never):Int;
	inline function get_accessTime():Int
		return this.lastAccessTime().to(SECONDS).toInt();

	public var modificationTime(get,never):Int;
	inline function get_modificationTime():Int
		return this.lastModifiedTime().to(SECONDS).toInt();

	public var creationTime(get,never):Int;
	inline function get_creationTime():Int
		return this.creationTime().to(SECONDS).toInt();

	public var deviceNumber(get,never):Int;
	inline function get_deviceNumber():Int
		throw NotSupportedException.field();

	public var group(get,never):SystemGroup;
	inline function get_group():SystemGroup
		return this.group();

	public var user(get,never):SystemUser;
	inline function get_user():SystemUser
		return this.owner();

	public var inodeNumber(get,never):Int;
	inline function get_inodeNumber():Int
		throw NotSupportedException.field();

	public var mode(get,never):FileMode;
	inline function get_mode():FileMode {
		return this;
	}

	public var links(get,never):Int;
	inline function get_links():Int
		throw NotSupportedException.field();

	public var deviceType(get,never):Int;
	inline function get_deviceType():Int
		throw NotSupportedException.field();

	public var size(get,never):Int;
	inline function get_size():Int
		return this.size().toInt();

	public var blockSize(get,never):Int;
	inline function get_blockSize():Int
		throw NotSupportedException.field();

	public var blocks(get,never):Int;
	inline function get_blocks():Int
		throw NotSupportedException.field();
}