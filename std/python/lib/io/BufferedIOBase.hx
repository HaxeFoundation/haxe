
package python.lib.io;

import python.lib.io.RawIOBase;
import python.lib.ByteArray;

extern class BufferedIOBase extends IOBase {

	/* not always available */
	public var raw:RawIOBase;

	public function write (b:ByteArray):Int;
	public function readinto (b:ByteArray):Int;
}