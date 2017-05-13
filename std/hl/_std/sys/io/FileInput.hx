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
import sys.io.File;

@:coreApi class FileInput extends haxe.io.Input {

	private var __f : FileHandle;

	function new(f:FileHandle) : Void {
		__f = f;
	}

	public override function readByte() : Int {
		var c = file_read_char(__f);
		if( c < 0 ) throw new haxe.io.Eof();
		return c;
	}

	public override function readBytes( s : haxe.io.Bytes, p : Int, l : Int ) : Int {
		if( p < 0 || l < 0 || p + l > s.length ) throw haxe.io.Error.OutsideBounds;
		var v = file_read(__f, s.getData(), p, l);
		if( v <= 0 ) throw new haxe.io.Eof();
		return v;
	}

	public override function close() : Void {
		super.close();
		file_close(__f);
		__f = null;
	}

	public function seek( p : Int, pos : FileSeek ) : Void {
		if( !file_seek(__f,p,switch( pos ) { case SeekBegin: 0; case SeekCur: 1; case SeekEnd: 2; }) )
			throw haxe.io.Error.Custom("seek() failure");
	}

	public function tell() : Int {
		var p = file_tell(__f);
		if( p < 0 )  throw haxe.io.Error.Custom("tell() failure");
		return p;
	}

	public function eof() : Bool {
		return file_eof(__f);
	}

	@:hlNative("std", "file_eof") static function file_eof( f : FileHandle ) : Bool { return false; }
	@:hlNative("std", "file_read") static function file_read( f : FileHandle, bytes : hl.Bytes, pos : Int, len : Int ) : Int { return 0; }
	@:hlNative("std", "file_read_char") static function file_read_char( f : FileHandle ) : Int { return 0; }
	@:hlNative("std", "file_close") static function file_close( f : FileHandle ) : Void { }
	@:hlNative("std", "file_seek") static function file_seek( f : FileHandle, pos : Int, from : Int ) : Bool { return true; }
	@:hlNative("std", "file_tell") static function file_tell( f : FileHandle ) : Int { return 0; }

}
