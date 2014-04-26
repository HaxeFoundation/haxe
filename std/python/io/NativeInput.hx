
package python.io;

import haxe.io.Eof;
import haxe.io.Input;

import python.lib.Builtin;
import python.lib.io.RawIOBase;
import python.lib.io.IOBase.SeekSet;


class NativeInput extends Input{

	var stream:RawIOBase;
	var wasEof:Bool;

	public var canSeek(get_canSeek, null):Bool;

	public function new (stream:RawIOBase) {
		this.stream = stream;
		wasEof = false;
		if (!stream.readable()) throw "Write-only stream";
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

	override public function readByte():Int
	{

		var ret = stream.read(1);

		if (ret.length == 0) throwEof();

		return ret.get(0);
	}

	public function seek( p : Int, pos : sys.io.FileSeek ) : Void
	{
		var pos = switch(pos)
		{
			case SeekBegin: SeekSet.SeekSet;
			case SeekCur: SeekSet.SeekCur;
			case SeekEnd: SeekSet.SeekEnd;
		};
		wasEof = false;
		stream.seek(p, pos);
	}


	override public function readBytes(s:haxe.io.Bytes, pos:Int, len:Int):Int
	{
		if( pos < 0 || len < 0 || pos + len > s.length )
			throw haxe.io.Error.OutsideBounds;

		stream.seek(pos, python.lib.io.IOBase.SeekSet.SeekCur);
		var ba = Builtin.bytearray(len);
		var ret = stream.readinto(ba);
		s.blit(pos, haxe.io.Bytes.ofData(ba) ,0,len);
		if (ret == 0)
			throwEof();
		return ret;
	}

	function throwEof() {
		wasEof = true;
		throw new Eof();
	}
}