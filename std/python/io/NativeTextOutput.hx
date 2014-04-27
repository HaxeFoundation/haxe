
package python.io;

import haxe.io.Output;

import python.io.IoTools;
import python.lib.Builtin;
import python.lib.io.IOBase;
import python.lib.io.RawIOBase;
import python.lib.io.TextIOBase;

class NativeTextOutput extends NativeOutput<TextIOBase> {

	public function new (stream:TextIOBase) {
		super(stream);
		if (!stream.writable()) throw "Read only stream";
	}

	public function seek( p : Int, pos : sys.io.FileSeek ) : Void
	{
		IoTools.seekInTextMode(stream, tell, p, pos);
	}

	override public function writeByte(c:Int):Void
	{
		stream.write(String.fromCharCode(c));
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