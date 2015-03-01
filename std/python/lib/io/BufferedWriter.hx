
package python.lib.io;

import python.lib.Bytearray;
import python.lib.io.BufferedIOBase;

@:pythonImport("io", "BufferedWriter")
extern class BufferedWriter extends BufferedIOBase implements IBufferedWriter {
	public function new (raw:RawIOBase):Void;
}


@:remove extern interface IBufferedWriter extends IBufferedIOBase {
	public function flush():Void;
}