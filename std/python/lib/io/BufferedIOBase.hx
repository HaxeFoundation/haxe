package python.lib.io;

import python.lib.io.IOBase;

import python.lib.io.RawIOBase;
import python.lib.Bytearray;

@:pythonImport("io", "BufferedIOBase")
extern class BufferedIOBase extends IOBase implements IBufferedIOBase {

	/* not always available */
	public var raw:RawIOBase;

	public function write (b:Bytearray):Int;
	public function readinto (b:Bytearray):Int;
	public function detach ():RawIOBase;
	public function read(n:Int = -1):Null<Bytes>;
	public function read1(n:Int = -1):Null<Bytes>;
}


@:remove extern interface IBufferedIOBase extends IIOBase {
	public var raw:RawIOBase;

	public function write (b:Bytearray):Int;
	public function readinto (b:Bytearray):Int;
	public function detach ():RawIOBase;
	public function read(n:Int = -1):Null<Bytes>;
	public function read1(n:Int = -1):Null<Bytes>;
}