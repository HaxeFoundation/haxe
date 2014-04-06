
package python.lib.io;

@:enum abstract SeekSet(Int) {
	var SeekSet = 0;
	var SeekCur = 1;
	var SeekEnd = 2;
}

extern class IOBase {

	public function close():Void;
	public function flush():Void;
	public function readline(limit:Int = -1):String;
	public function readable():Bool;
	public var closed(default, null):Bool;
	public function readlines(hint:Int=-1):Array<String>;
	public function tell():Int;
	public function writable():Bool;
	public function seekable():Bool;
	public function fileno():Int;
	public function seek(offset:Int, whence:SeekSet):Int;
	public function truncate (size:Int):Int;
}