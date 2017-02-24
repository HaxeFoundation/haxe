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

@:coreApi class FileOutput extends haxe.io.Output {

	private var __f : FileHandle;

	function new(f:FileHandle) : Void {
		__f = f;
	}

	public override function writeByte( c : Int ) : Void {
		if( !file_write_char(__f, c) ) throw new haxe.io.Eof();
	}

	public override function writeBytes( s : haxe.io.Bytes, p : Int, l : Int ) : Int {
		if( p < 0 || l < 0 || p + l > s.length ) throw haxe.io.Error.OutsideBounds;
		var v = file_write(__f, s.getData(), p, l);
		if( v <= 0 ) throw new haxe.io.Eof();
		return v;
	}

	public override function flush() : Void {
		if( !file_flush(__f) ) throw haxe.io.Error.Custom("flush() failure");
	}

	public override function close() : Void {
		super.close();
		@:privateAccess FileInput.file_close(__f);
		__f = null;
	}

	public function seek( p : Int, pos : FileSeek ) : Void {
		if( @:privateAccess !FileInput.file_seek(__f,p,switch( pos ) { case SeekBegin: 0; case SeekCur: 1; case SeekEnd: 2; }) )
			throw haxe.io.Error.Custom("seek() failure");
	}

	public function tell() : Int {
		var p = @:privateAccess FileInput.file_tell(__f);
		if( p < 0 )  throw haxe.io.Error.Custom("tell() failure");
		return p;
	}

	@:hlNative("std","file_flush") static function file_flush( f : FileHandle ) : Bool { return true; }
	@:hlNative("std", "file_write") static function file_write( f : FileHandle, bytes : hl.Bytes, pos : Int, len : Int ) : Int { return 0; }
	@:hlNative("std", "file_write_char") static function file_write_char( f : FileHandle, v : Int ) : Bool { return true; }

}
