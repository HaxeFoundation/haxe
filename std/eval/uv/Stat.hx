package eval.uv;

import asys.FilePermissions;

class Stat {
	extern function get_dev():Int;

	public var dev(get, never):Int;

	extern function get_mode():Int;

	public var mode(get, never):Int;

	extern function get_nlink():Int;

	public var nlink(get, never):Int;

	extern function get_uid():Int;

	public var uid(get, never):Int;

	extern function get_gid():Int;

	public var gid(get, never):Int;

	extern function get_rdev():Int;

	public var rdev(get, never):Int;

	extern function get_ino():Int;

	public var ino(get, never):Int;

	extern function get_size():Int;

	public var size(get, never):Int;

	extern function get_blksize():Int;

	public var blksize(get, never):Int;

	extern function get_blocks():Int;

	public var blocks(get, never):Int;

	extern function get_flags():Int;

	public var flags(get, never):Int;

	extern function get_gen():Int;

	public var gen(get, never):Int;

	public function isBlockDevice():Bool return (mode & asys.uv.UVConstants.S_IFMT) == asys.uv.UVConstants.S_IFBLK;

	public function isCharacterDevice():Bool return (mode & asys.uv.UVConstants.S_IFMT) == asys.uv.UVConstants.S_IFCHR;

	public function isDirectory():Bool return (mode & asys.uv.UVConstants.S_IFMT) == asys.uv.UVConstants.S_IFDIR;

	public function isFIFO():Bool return (mode & asys.uv.UVConstants.S_IFMT) == asys.uv.UVConstants.S_IFIFO;

	public function isFile():Bool return (mode & asys.uv.UVConstants.S_IFMT) == asys.uv.UVConstants.S_IFREG;

	public function isSocket():Bool return (mode & asys.uv.UVConstants.S_IFMT) == asys.uv.UVConstants.S_IFSOCK;

	public function isSymbolicLink():Bool return (mode & asys.uv.UVConstants.S_IFMT) == asys.uv.UVConstants.S_IFLNK;

	function get_permissions():FilePermissions return @:privateAccess new FilePermissions(mode & asys.uv.UVConstants.S_PERM);

	public var permissions(get, never):FilePermissions;
}
