
package sys.io;

import python.lib.io.RawIOBase;

class FileOutput extends python.io.NativeOutput {

	public function new (stream:RawIOBase) {
		super(stream);
	}
}