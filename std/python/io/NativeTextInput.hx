
package python.io;

import haxe.io.Eof;
import haxe.io.Input;

import python.io.IInput;
import python.io.NativeInput;
import python.lib.Builtin;
import python.lib.io.RawIOBase;
import python.lib.io.IOBase.SeekSet;
import python.lib.io.TextIOBase;


class NativeTextInput extends NativeInput<TextIOBase> implements IInput {

	public function new (stream:TextIOBase) {
		super(stream);
	}

	override public function readByte():Int
	{
		var ret = stream.read(1);

		if (ret.length == 0) throwEof();

		return ret.charCodeAt(0);
	}

	public function seek( p : Int, pos : sys.io.FileSeek ) : Void
	{
		wasEof = false;


 		var pos = switch (pos) {
 			case SeekBegin:
 				SeekSet.SeekSet;
 			case SeekCur:
 				p = tell() + p;
 				SeekSet.SeekSet;
 			case SeekEnd :
 				stream.seek(0, SeekSet.SeekEnd);
 				p = tell() + p;
 				SeekSet.SeekSet;
 		}
 		stream.seek(p, pos);

	}


	override public function readBytes(s:haxe.io.Bytes, pos:Int, len:Int):Int
	{
		throw "not implemented";
		/*
		if( pos < 0 || len < 0 || pos + len > s.length )
			throw haxe.io.Error.OutsideBounds;

		stream.seek(pos, python.lib.io.IOBase.SeekSet.SeekCur);
		var ba = Builtin.bytearray(len);
		var ret = stream.readinto(ba);
		s.blit(pos, haxe.io.Bytes.ofData(ba) ,0,len);
		if (ret == 0)
			throwEof();
		return ret;
		*/
	}

}