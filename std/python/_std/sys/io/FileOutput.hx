package sys.io;

import haxe.io.Bytes;
import haxe.io.Input;
import haxe.io.Output;
import python.io.IFileOutput;

class FileOutput extends Output {

	var impl:IFileOutput;

	public function new (impl:IFileOutput) {
		this.impl = impl;
	}

	public function seek( p : Int, pos : FileSeek ) : Void {
		return impl.seek(p, pos);
	}

	public function tell() : Int {
		return impl.tell();
	}

	override public function set_bigEndian(b:Bool) {
		return impl.bigEndian = b;
	}

	override public function writeByte( c : Int ) : Void {
		impl.writeByte(c);
	}

	override public function writeBytes( s : Bytes, pos : Int, len : Int ):Int {
		return impl.writeBytes(s,pos,len);
	}

	override public function flush():Void {
		impl.flush();
	}

	override public function close():Void {
		impl.close();
	}

	override public function write( s : Bytes ) : Void {
		impl.write(s);
	}

	override public function writeFullBytes( s : Bytes, pos : Int, len : Int ):Void {
		impl.writeFullBytes(s,pos,len);
	}

	override public function writeFloat( x : Float ):Void {
		impl.writeFloat(x);
	}

	override public function writeDouble( x : Float ):Void {
		impl.writeDouble(x);
	}

	override public function writeInt8( x : Int ):Void {
		impl.writeInt8(x);
	}

	override public function writeInt16( x : Int ):Void {
		impl.writeInt16(x);
	}

	override public function writeUInt16( x : Int ):Void {
		impl.writeUInt16(x);
	}

	override public function writeInt24( x : Int ):Void {
		impl.writeInt24(x);
	}

	override public function writeUInt24( x : Int ):Void {
		impl.writeUInt24(x);
	}

	override public function writeInt32( x : Int ):Void {
		impl.writeInt32(x);
	}

	override public function prepare( nbytes : Int ):Void {
		impl.prepare(nbytes);
	}

	override public function writeInput( i : Input, ?bufsize : Int ):Void {
		impl.writeInput(i,bufsize);
	}

	override public function writeString( s : String ):Void {
		impl.writeString(s);
	}
}