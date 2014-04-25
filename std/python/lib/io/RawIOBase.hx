
package python.lib.io;

import python.lib.io.FileIO;
import python.lib.io.IOBase;

extern class RawIOBase extends IOBase{

	public function readall():Bytes;
	public function read(n:Int=-1):Null<Bytes>;
	public function write(b:ByteArray):Null<Int>;
	public function readinto(b:ByteArray):Null<Int>;
}