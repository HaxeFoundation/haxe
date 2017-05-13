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
import python.io.IFileInput;



class FileInput extends Input
{
	var impl:IFileInput;



	public function new (impl:IFileInput) {
		this.impl = impl;
	}

	override public function set_bigEndian(b:Bool) {
		return impl.bigEndian = b;
	}

	public function seek( p : Int, pos : FileSeek ) : Void {
		return impl.seek(p, pos);
	}
	public function tell() : Int {
		return impl.tell();
	}
	public function eof() : Bool {
		return impl.eof();
	}

	override public function readByte() : Int {
		return impl.readByte();
	}

	override public function readBytes( s : Bytes, pos : Int, len : Int ) : Int {
		return impl.readBytes(s, pos, len);
	}

	override public function close():Void {
		impl.close();
	}

	override public function readAll( ?bufsize : Int ) : Bytes {
		return impl.readAll(bufsize);
	}

	override public function readFullBytes( s : Bytes, pos : Int, len : Int ):Void {
		impl.readFullBytes(s, pos, len);
	}

	override public function read( nbytes : Int ) : Bytes {
		return impl.read(nbytes);
	}

	override public function readUntil( end : Int ) : String {
		return impl.readUntil(end);
	}

	override public function readLine() : String {
		return impl.readLine();
	}

	override public function readFloat() : Float {
		return impl.readFloat();
	}

	override public function readDouble() : Float {
		return impl.readDouble();
	}

	override public function readInt8():Int {
		return impl.readInt8();
	}

	override public function readInt16():Int {
		return impl.readInt16();
	}

	override public function readUInt16():Int {
		return impl.readUInt16();
	}

	override public function readInt24():Int {
		return impl.readInt24();
	}

	override public function readUInt24():Int {
		return impl.readUInt24();
	}

	override public function readInt32():Int {
		return impl.readInt32();
	}

	override public function readString( len : Int ) : String {
		return impl.readString(len);
	}




}