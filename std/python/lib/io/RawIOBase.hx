
package python.lib.io;

import python.lib.io.FileIO;
import python.lib.io.IOBase;

@:pythonImport("io", "RawIOBase")
extern class RawIOBase extends IOBase implements IRawIOBase {

	public function readall():Bytes;
	public function read(n:Int=-1):Null<Bytes>;
	public function write(b:Bytearray):Null<Int>;
	public function readinto(b:Bytearray):Null<Int>;
}

@:remove extern interface IRawIOBase extends IIOBase {

	public function readall():Bytes;
	public function read(n:Int=-1):Null<Bytes>;
	public function write(b:Bytearray):Null<Int>;
	public function readinto(b:Bytearray):Null<Int>;
}
