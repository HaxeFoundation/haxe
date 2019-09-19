package eval.uv;

import haxe.io.FilePath;

class DirectoryEntry implements asys.DirectoryEntry {
	public var name(get, never):FilePath;

	extern function get_type():asys.uv.UVDirentType;

	extern function get_name():FilePath;

	public function isBlockDevice():Bool return get_type() == DirentBlock;

	public function isCharacterDevice():Bool return get_type() == DirentChar;

	public function isDirectory():Bool return get_type() == DirentDir;

	public function isFIFO():Bool return get_type() == DirentFifo;

	public function isFile():Bool return get_type() == DirentFile;

	public function isSocket():Bool return get_type() == DirentSocket;

	public function isSymbolicLink():Bool return get_type() == DirentLink;
}
