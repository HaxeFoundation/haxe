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

@:coreApi class FileOutput extends haxe.io.Output {

	private var __f : File.FileHandle;

	function new(f:File.FileHandle) : Void {
		__f = f;
	}

	public override function writeByte( c : Int ) : Void {
		try file_write_char(__f,c) catch( e : Dynamic ) throw haxe.io.Error.Custom(e);
	}

	public override function writeBytes( s : haxe.io.Bytes, p : Int, l : Int ) : Int {
		return try file_write(__f,s.getData(),p,l) catch( e : Dynamic ) throw haxe.io.Error.Custom(e);
	}

	public override function flush() : Void {
		file_flush(__f);
	}

	public override function close() : Void {
		super.close();
		file_close(__f);
	}

	public function seek( p : Int, pos : FileSeek ) : Void {
		file_seek(__f,p,switch( pos ) { case SeekBegin: 0; case SeekCur: 1; case SeekEnd: 2; });
	}

	public function tell() : Int {
		return file_tell(__f);
	}

	private static var file_close = neko.Lib.load("std","file_close",1);
	private static var file_seek = neko.Lib.load("std","file_seek",3);
	private static var file_tell = neko.Lib.load("std","file_tell",1);

	private static var file_flush = neko.Lib.load("std","file_flush",1);
	private static var file_write = neko.Lib.load("std","file_write",4);
	private static var file_write_char = neko.Lib.load("std","file_write_char",2);

}
