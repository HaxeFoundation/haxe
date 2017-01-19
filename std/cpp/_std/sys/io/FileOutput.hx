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
import sys.io.FileSeek;
import cpp.NativeFile;

@:coreApi
class FileOutput extends haxe.io.Output {

	private var __f : Dynamic;

	function new(f:Dynamic) : Void {
		__f = f;
	}

	public override function writeByte( c : Int ) : Void {
		try NativeFile.file_write_char(__f,c) catch( e : Dynamic ) throw haxe.io.Error.Custom(e);
	}

	public override function writeBytes( s : haxe.io.Bytes, p : Int, l : Int ) : Int {
		return try NativeFile.file_write(__f,s.getData(),p,l) catch( e : Dynamic ) throw haxe.io.Error.Custom(e);
	}

	public override function flush() : Void {
		NativeFile.file_flush(__f);
	}

	public override function close() : Void {
		super.close();
		NativeFile.file_close(__f);
	}

	public function seek( p : Int, pos : FileSeek ) : Void {
		NativeFile.file_seek(__f,p, pos == SeekBegin ? 0 : pos ==  SeekCur ? 1 : 2);
	}

	public function tell() : Int {
		return NativeFile.file_tell(__f);
	}

}
