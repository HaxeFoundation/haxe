package asys.uv;

class UVStat {
	public final dev:Int;
	public final mode:Int;
	public final nlink:Int;
	public final uid:Int;
	public final gid:Int;
	public final rdev:Int;
	public final ino:Int;
	public final size:Int;
	public final blksize:Int;
	public final blocks:Int;
	public final flags:Int;
	public final gen:Int;

	public function new(st_dev:Int, st_mode:Int, st_nlink:Int, st_uid:Int, st_gid:Int, st_rdev:Int, st_ino:Int, st_size:Int, st_blksize:Int, st_blocks:Int,
			st_flags:Int, st_gen:Int) {
		dev = st_dev;
		mode = st_mode;
		nlink = st_nlink;
		uid = st_uid;
		gid = st_gid;
		rdev = st_rdev;
		ino = st_ino;
		size = st_size;
		blksize = st_blksize;
		blocks = st_blocks;
		flags = st_flags;
		gen = st_gen;
	}

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
