
package python.io;

import haxe.io.Bytes;
import haxe.io.Input;

interface IOutput {

	public var bigEndian(default, set) : Bool;

	public function writeByte( c : Int ) : Void;

	public function writeBytes( s : Bytes, pos : Int, len : Int ):Int;

	public function flush():Void;

	public function close():Void;

	public function write( s : Bytes ) : Void;

	public function writeFullBytes( s : Bytes, pos : Int, len : Int ):Void;

	public function writeFloat( x : Float ):Void;

	public function writeDouble( x : Float ):Void;

	public function writeInt8( x : Int ):Void;

	public function writeInt16( x : Int ):Void;

	public function writeUInt16( x : Int ):Void;

	public function writeInt24( x : Int ):Void;

	public function writeUInt24( x : Int ):Void;

	public function writeInt32( x : Int ):Void;

	public function prepare( nbytes : Int ):Void;

	public function writeInput( i : Input, ?bufsize : Int ):Void;

	public function writeString( s : String ):Void;
}