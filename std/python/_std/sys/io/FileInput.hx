
package sys.io;

import python.io.NativeInput;
import python.lib.io.RawIOBase;
import python.lib.io.IOBase.SeekSet;

class FileInput extends NativeInput {

	public function new (stream:RawIOBase) {
		super(stream);
	}

	public function eof() {
		return wasEof;
	}
}