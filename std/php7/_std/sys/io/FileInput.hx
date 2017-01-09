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

import haxe.io.Eof;
import haxe.io.Error;
import haxe.io.Bytes;
import php.*;
import php.Global.*;
import php.Const.*;

@:coreApi
class FileInput extends haxe.io.Input {

	private var __f : Resource;

	function new(f:Resource) : Void {
		__f = f;
	}

	public override function readByte() : Int {
		var r = fread(__f, 1);
		if(feof(__f)) throw new Eof();
		if(r == false) throw Custom('An error occurred');
		return ord(r);
	}

	public override function readBytes( s : Bytes, p : Int, l : Int ) : Int {
		if(feof(__f)) throw new Eof();
		var r = fread(__f, l);
		if(r == false) throw Custom('An error occurred');
		var b = Bytes.ofString(r);
		s.blit(p, b, 0, (r:String).length);
		return (r:String).length;
	}

	public override function close() : Void {
		super.close();
		if(__f != null)	fclose(__f);
	}

	public function seek( p : Int, pos : FileSeek ) : Void {
		var w;
		switch( pos ) {
			case SeekBegin: w = SEEK_SET;
			case SeekCur  : w = SEEK_CUR;
			case SeekEnd  : w = SEEK_END;
		}
		var r = fseek(__f, p, w);
		if(r == false) throw Custom('An error occurred');
	}

	public function tell() : Int {
		var r = ftell(__f);
		if(r == false) throw Custom('An error occurred');
		return cast r;
	}

	public function eof() : Bool {
		return feof(__f);
	}

	override function readLine() : String {
		var r = fgets(__f);
		if (false == r) throw new Eof();
		return rtrim(r, "\r\n");
	}
}
