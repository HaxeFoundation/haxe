
package python.io;

import python.io.FileBytesInput;
import python.io.FileTextInput;
import python.io.FileTextOutput;
import python.io.FileBytesOutput;
import python.lib.io.RawIOBase;
import python.lib.io.TextIOBase;
import sys.io.FileInput;
import sys.io.FileOutput;

import python.lib.io.IOBase.SeekSet;


class IoTools {

	public static function createFileInputFromText (t:TextIOBase) {
		return new FileInput(new FileTextInput(t));
	}

	public static function createFileInputFromBytes (t:RawIOBase) {
		return new FileInput(new FileBytesInput(t));
	}

	public static function createFileOutputFromText (t:TextIOBase) {
		return new FileOutput(new FileTextOutput(t));
	}

	public static function createFileOutputFromBytes (t:RawIOBase) {
		return new FileOutput(new FileBytesOutput(t));
	}

	public static function seekInTextMode (stream:TextIOBase, tell:Void->Int , p : Int, pos : sys.io.FileSeek)
	{
 		var pos = switch (pos) {
 			case SeekBegin:
 				SeekSet;
 			case SeekCur:
 				p = tell() + p;
 				SeekSet;
 			case SeekEnd :
 				stream.seek(0, SeekEnd);
 				p = tell() + p;
 				SeekSet;
 		}
 		stream.seek(p, pos);
	}

	public static function seekInBinaryMode (stream:RawIOBase, p : Int, pos : sys.io.FileSeek)
	{
 		var pos = switch(pos)
		{
			case SeekBegin: SeekSet;
			case SeekCur: SeekCur;
			case SeekEnd: SeekEnd;
		};
		stream.seek(p, pos);
	}

}