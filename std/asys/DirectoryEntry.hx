package asys;

import haxe.io.FilePath;

/**
	An entry returned from `asys.FileSystem.readdirTypes`.
**/
interface DirectoryEntry {
	var name(get, never):FilePath;
	function isBlockDevice():Bool;
	function isCharacterDevice():Bool;
	function isDirectory():Bool;
	function isFIFO():Bool;
	function isFile():Bool;
	function isSocket():Bool;
	function isSymbolicLink():Bool;
}
