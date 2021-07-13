package asys.native.filesystem;

import haxe.exceptions.NotSupportedException;

private typedef NativeMode = java.nio.file.attribute.PosixFileAttributes;

@:coreApi
abstract FileMode(NativeMode) from NativeMode {

	public function has(permissions:FilePermissions):Bool
		return this.permissions().containsAll(permissions);

	public inline function isBlockDevice():Bool
		throw NotSupportedException.field();

	public inline function isCharacterDevice():Bool
		throw NotSupportedException.field();

	public inline function isDirectory():Bool
		return this.isDirectory();

	public inline function isFIFO():Bool
		throw NotSupportedException.field();

	public inline function isFile():Bool
		return this.isRegularFile();

	public inline function isSocket():Bool
		throw NotSupportedException.field();

	public inline function isLink():Bool
		return this.isSymbolicLink();
}