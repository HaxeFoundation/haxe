
package python.io;

import haxe.io.Bytes;

interface IInput
{
	public var bigEndian(default,set) : Bool;

	public function readByte() : Int;

	public function readBytes( s : Bytes, pos : Int, len : Int ) : Int;

	public function close():Void;

	public function readAll( ?bufsize : Int ) : Bytes;

	public function readFullBytes( s : Bytes, pos : Int, len : Int ):Void;

	public function read( nbytes : Int ) : Bytes;

	public function readUntil( end : Int ) : String;

	public function readLine() : String;

	public function readFloat() : Float;

	public function readDouble() : Float;

	public function readInt8():Int;

	public function readInt16():Int;

	public function readUInt16():Int;

	public function readInt24():Int;

	public function readUInt24():Int;

	public function readInt32():Int;

	public function readString( len : Int ) : String;
}