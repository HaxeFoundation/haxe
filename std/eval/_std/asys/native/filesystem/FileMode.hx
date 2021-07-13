package asys.native.filesystem;

import eval.luv.File;
import haxe.exceptions.NotSupportedException;

private typedef NativeMode = FileModeNumeric;

@:coreApi
abstract FileMode(NativeMode) from NativeMode {

	public inline function has(permissions:FilePermissions):Bool {
		return File.testMode(permissions, this);
	}

	public inline function isBlockDevice():Bool
		return File.testMode([IFBLK], this);

	public inline function isCharacterDevice():Bool
		return File.testMode([IFCHR], this);

	public inline function isDirectory():Bool
		return File.testMode([IFDIR], this);

	public inline function isFIFO():Bool
		return File.testMode([IFIFO], this);

	public inline function isFile():Bool
		return File.testMode([IFREG], this) && !File.testMode([IFLNK], this);

	public inline function isSocket():Bool
		throw NotSupportedException.field();

	public inline function isLink():Bool
		return File.testMode([IFLNK], this);
}