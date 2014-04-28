
package python.io;

import python.io.NativeBytesInput;
import python.io.NativeTextInput;
import python.lib.io.RawIOBase;
import python.lib.io.IOBase.SeekSet;
import python.lib.io.TextIOBase;

class FileBytesInput extends NativeBytesInput implements IFileInput {
	public function new (stream:RawIOBase) {
		super(stream);
	}
}