
package python.lib.io;

import python.lib.Bytes;
import python.lib.io.BufferedIOBase;

@:pythonImport("io", "BufferedReader")
extern class BufferedReader extends BufferedIOBase implements IBufferedReader {

	public function new (raw:RawIOBase):Void;

	public function peek(?n:Int):Null<Bytes>;


}

@:remove extern interface IBufferedReader extends IBufferedIOBase {
	public function peek(?n:Int):Null<Bytes>;

}