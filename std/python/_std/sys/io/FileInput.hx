
package sys.io;

import python.io.NativeInput;
import python.lib.io.RawIOBase;

class FileInput extends NativeInput {
	public function new (stream:RawIOBase) {
		super(stream);
	}



}