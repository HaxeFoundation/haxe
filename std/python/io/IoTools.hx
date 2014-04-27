
package python.io;

import python.io.FileBytesInput;
import python.io.FileTextInput;
import python.io.FileTextOutput;
import python.io.FileBytesOutput;
import python.lib.io.RawIOBase;
import python.lib.io.TextIOBase;
import sys.io.FileInput;
import sys.io.FileOutput;

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

}