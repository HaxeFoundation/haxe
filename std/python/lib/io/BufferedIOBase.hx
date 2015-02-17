
package python.lib.io;

import python.lib.io.RawIOBase;
import python.lib.Bytearray;

extern class BufferedIOBase extends IOBase {

	/* not always available */
	public var raw:RawIOBase;

	public function write (b:Bytearray):Int;
	public function readinto (b:Bytearray):Int;
}