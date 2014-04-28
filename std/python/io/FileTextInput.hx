package python.io;

import python.io.NativeBytesInput;
import python.io.NativeTextInput;
import python.lib.io.RawIOBase;
import python.lib.io.IOBase.SeekSet;
import python.lib.io.TextIOBase;

class FileTextInput extends NativeTextInput implements IFileInput {
	public function new (stream:TextIOBase) {
		super(stream);
	}
}
