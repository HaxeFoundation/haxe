
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

}