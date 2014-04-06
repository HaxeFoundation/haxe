
package python.io;

import haxe.io.Output;

import python.lib.Builtin;
import python.lib.io.IOBase;
import python.lib.io.RawIOBase;

class NativeOutput extends Output{

	var stream:RawIOBase;

	public var canSeek(get_canSeek, null):Bool;

	public function new (stream:RawIOBase) {
		this.stream = stream;
		if (!stream.writable()) throw "Read only stream";
	}

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



	public function seek( p : Int, pos : sys.io.FileSeek ) : Void
	{
		var pos = switch(pos)
		{
			case SeekBegin: SeekSet.SeekSet;
			case SeekCur: SeekSet.SeekCur;
			case SeekEnd: SeekSet.SeekEnd;
		};
		stream.seek(p, pos);
	}

	override public function prepare(nbytes:Int):Void
	{
		stream.truncate(nbytes);
	}

	override public function flush():Void
	{
		stream.flush();
	}

	override public function writeByte(c:Int):Void
	{
		stream.write(Builtin.bytearray([c]));
	}

	/*
	** TODO not sure if this implementation is working

	override public function writeBytes( s : haxe.io.Bytes, pos : Int, len : Int ) : Int
	{
		if( pos < 0 || len < 0 || pos + len > s.length )
			throw haxe.io.Error.OutsideBounds;

		var ba = s.sub(pos, len).getData();

		var ret = stream.write(ba);

		return ret;
	}
	*/

}