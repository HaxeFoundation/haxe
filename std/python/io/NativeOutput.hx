
package python.io;

import haxe.io.Output;

import python.lib.Builtin;
import python.lib.io.IOBase;
import python.lib.io.RawIOBase;

class NativeOutput<T:IOBase> extends Output {

	var stream:T;

	public var canSeek(get_canSeek, null):Bool;

	public function new (stream:T) {
		this.stream = stream;
		if (!stream.writable()) throw "Read only stream";
	}

	override public function close():Void
	{
		stream.close();
	}

	private function get_canSeek():Bool
	{
		return stream.seekable();
	}

	override public function prepare(nbytes:Int):Void
	{
		stream.truncate(nbytes);
	}

	override public function flush():Void
	{
		stream.flush();
	}


	public function tell() : Int
	{
		return stream.tell();
	}
}