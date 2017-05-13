/*
 * Copyright (C)2005-2017 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */
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