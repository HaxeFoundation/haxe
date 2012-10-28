/*
 * Copyright (C)2005-2012 Haxe Foundation
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
import haxe.io.Eof;

@:coreApi
class FileInput extends haxe.io.Input {

	private var __f : File.FileHandle;

	function new(f:File.FileHandle) : Void {
		__f = f;
	}

	public override function readByte() : Int {
		if(untyped __call__('feof', __f)) return throw new haxe.io.Eof();
		var r = untyped __call__('fread', __f, 1);
		if(untyped __physeq__(r, false)) return throw haxe.io.Error.Custom('An error occurred');
		return untyped __call__('ord', r);
	}

	public override function readBytes( s : haxe.io.Bytes, p : Int, l : Int ) : Int {
		if(untyped __call__('feof', __f)) return throw new haxe.io.Eof();
		var r : String = untyped __call__('fread', __f, l);
		if(untyped __physeq__(r, false)) return throw haxe.io.Error.Custom('An error occurred');
		var b = haxe.io.Bytes.ofString(r);
		s.blit(p, b, 0, r.length);
		return r.length;
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

	public function eof() : Bool {
		return untyped __call__('feof', __f);
	}

	override function readLine() : String {
		var r : String = untyped __call__('fgets', __f);
		if (untyped __physeq__(false, r))
			throw new Eof();
		return untyped __call__("rtrim", r, "\r\n");
	}
}
