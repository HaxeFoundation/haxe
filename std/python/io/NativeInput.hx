
package python.io;

import haxe.io.Eof;
import haxe.io.Input;
import python.lib.io.IOBase;

class NativeInput<T:IOBase> extends Input{

	var stream:T;
	var wasEof:Bool;

	function new (s:T) {
		this.stream = s;
		wasEof = false;
		if (!stream.readable()) throw "Write-only stream";
	}

	public var canSeek(get_canSeek, null):Bool;

	private function get_canSeek():Bool
	{
		return stream.seekable();
	}

	override public function close():Void
	{
		stream.close();
	}

	public function tell() : Int
	{
		return stream.tell();
	}

	function throwEof() {
		wasEof = true;
		throw new Eof();
	}

	public function eof() {
		return wasEof;
	}
}