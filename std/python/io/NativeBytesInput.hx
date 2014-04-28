
package python.io;

import haxe.io.Eof;
import haxe.io.Input;

import python.io.IInput;
import python.io.IoTools;
import python.lib.Builtin;
import python.lib.ByteArray;
import python.lib.io.RawIOBase;
import python.lib.io.IOBase.SeekSet;


class NativeBytesInput extends NativeInput<RawIOBase> implements IInput {


	public function new (stream:RawIOBase) {
		super(stream);

	}

	override public function readByte():Int
	{

		var ret = stream.read(1);

		if (ret.length == 0) throwEof();

		return ret.get(0);
	}

	public function seek( p : Int, pos : sys.io.FileSeek ) : Void
	{
		wasEof = false;
		return IoTools.seekInBinaryMode(stream, p, pos);
	}

	override function readinto (b:ByteArray):Int {
		return stream.readinto(b);
	}


}