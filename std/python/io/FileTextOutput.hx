
package python.io;

import python.io.IFileOutput;
import python.io.NativeBytesOutput;
import python.lib.io.TextIOBase;

class FileTextOutput extends NativeTextOutput implements IFileOutput {
	public function new (stream:TextIOBase) {
		super(stream);
	}
}