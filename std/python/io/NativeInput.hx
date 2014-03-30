
package python.io;

import haxe.io.Eof;
import haxe.io.Input;

import python.lib.io.RawIOBase;
import python.lib.io.IOBase.SeekSet;


class NativeInput extends Input{

	var stream:RawIOBase;

	public var canSeek(get_canSeek, null):Bool;

	public function new (stream:RawIOBase) {
		this.stream = stream;
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
		trace(ret);
		if (ret.length == 0) throw new Eof();

		return ret.get(0);
	}

	public function seek( p : Int, pos : sys.io.FileSeek ) : Void
	{
		var pos = switch(pos)
		{
			case SeekBegin: SeekSet.SeekStart;
			case SeekCur: SeekSet.SeekCur;
			case SeekEnd: SeekSet.SeekEnd;
		};
		stream.seek(p, pos);
	}

	/*
	override public function readBytes(s:Bytes, pos:Int, len:Int):Int
	{
		if( pos < 0 || len < 0 || pos + len > s.length )
			throw Error.OutsideBounds;

		var ret = stream.Read(s.getData(), pos, len);
		if (ret == 0)
			throw new Eof();
		return ret;
	}
	*/

	/*

	public function eof() : Bool
	{
		return stream.Position == stream.Length;
	}
	*/
}