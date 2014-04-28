
package python.io;

import haxe.io.Output;

import python.lib.Builtin;
import python.lib.io.IOBase;
import python.lib.io.RawIOBase;

class NativeBytesOutput extends NativeOutput<RawIOBase>{

	public function new (stream:RawIOBase) {
		super(stream);
	}

	public function seek( p : Int, pos : sys.io.FileSeek ) : Void
	{
		return IoTools.seekInBinaryMode(stream, p, pos);
	}

	override public function prepare(nbytes:Int):Void
	{
		stream.truncate(nbytes);
	}

	override public function writeByte(c:Int):Void
	{
		stream.write(Builtin.bytearray([c]));
	}
}