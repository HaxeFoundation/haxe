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

@:coreApi class FileInput extends haxe.io.Input {

	private var __f : File.FileHandle;

	function new(f:File.FileHandle) : Void {
		__f = f;
	}

	public override function readByte() : Int {
		return try {
			file_read_char(__f);
		} catch( e : Dynamic ) {
			if( untyped __dollar__typeof(e) == __dollar__tarray )
				throw new haxe.io.Eof();
			else
				throw haxe.io.Error.Custom(e);
		}
	}

	public override function readBytes( s : haxe.io.Bytes, p : Int, l : Int ) : Int {
		return try {
			file_read(__f,s.getData(),p,l);
		} catch( e : Dynamic ) {
			if( untyped __dollar__typeof(e) == __dollar__tarray )
				throw new haxe.io.Eof();
			else
				throw haxe.io.Error.Custom(e);
		}
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


	public function eof() : Bool {
		return file_eof(__f);
	}

	private static var file_eof = neko.Lib.load("std","file_eof",1);

	private static var file_read = neko.Lib.load("std","file_read",4);
	private static var file_read_char = neko.Lib.load("std","file_read_char",1);

	private static var file_close = neko.Lib.load("std","file_close",1);
	private static var file_seek = neko.Lib.load("std","file_seek",3);
	private static var file_tell = neko.Lib.load("std","file_tell",1);

}
