
package python.io;

import haxe.io.Eof;
import haxe.io.Input;
import python.lib.Builtin;
import python.lib.ByteArray;
import python.lib.io.IOBase;
import python.lib.io.RawIOBase;

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

	function readinto (b:ByteArray):Int {
		throw "abstract method, should be overriden";
	}

	override public function readBytes(s:haxe.io.Bytes, pos:Int, len:Int):Int
	{
		if( pos < 0 || len < 0 || pos + len > s.length )
			throw haxe.io.Error.OutsideBounds;

		stream.seek(pos, python.lib.io.IOBase.SeekSet.SeekCur);
		var ba = Builtin.bytearray(len);
		var ret = readinto(ba);
		s.blit(pos, haxe.io.Bytes.ofData(ba) ,0,len);
		if (ret == 0)
			throwEof();
		return ret;
	}
}