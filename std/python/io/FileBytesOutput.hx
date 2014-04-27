
package python.io;

import python.io.IFileOutput;
import python.io.NativeBytesOutput;
import python.lib.io.RawIOBase;

class FileBytesOutput extends NativeBytesOutput implements IFileOutput {
	public function new (stream:RawIOBase) {
		super(stream);
	}
}