package python.lib.os;

extern class DirEntry {
	final name:String;
	final path:String;
	function inode():Int;
	function is_file():Bool;
	function is_dir():Bool;
	function is_symlink():Bool;
	function stat():python.lib.Os.Stat;
}