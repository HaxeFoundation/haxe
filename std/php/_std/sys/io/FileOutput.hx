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

@:coreApi
class FileOutput extends haxe.io.Output {
	private var __f : File.FileHandle;

	function new(f:File.FileHandle) : Void {
		__f = f;
	}

	public override function writeByte( c : Int ) : Void {
		var r = untyped __call__('fwrite', __f, __call__('chr', c));
		if(untyped __physeq__(r, false)) throw haxe.io.Error.Custom('An error occurred');
	}

	public override function writeBytes( b : haxe.io.Bytes, p : Int, l : Int ) : Int {
		var s = b.getString(p, l);
		if(untyped __call__('feof', __f)) return throw new haxe.io.Eof();
		var r = untyped __call__('fwrite', __f, s, l);
		if(untyped __physeq__(r, false)) return throw haxe.io.Error.Custom('An error occurred');
		return r;
	}

	public override function flush() : Void {
		var r = untyped __call__('fflush', __f);
		if(untyped __physeq__(r, false)) throw haxe.io.Error.Custom('An error occurred');
	}

	public override function close() : Void {
		super.close();
		if(__f != null)	untyped __call__('fclose', __f);
	}

	public function seek( p : Int, pos : FileSeek ) : Void {
		var w;
		switch( pos ) {
			case SeekBegin: w = untyped __php__('SEEK_SET');
			case SeekCur  : w = untyped __php__('SEEK_CUR');
			case SeekEnd  : w = untyped __php__('SEEK_END');
		}
		var r = untyped __call__('fseek', __f, p, w);
		if(untyped __physeq__(r, false)) throw haxe.io.Error.Custom('An error occurred');
	}

	public function tell() : Int {
		var r = untyped __call__('ftell', __f);
		if(untyped __physeq__(r, false)) return throw haxe.io.Error.Custom('An error occurred');
		return cast r;
	}

}
