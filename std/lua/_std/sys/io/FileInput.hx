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

import lua.FileHandle;
import lua.Io;
import lua.NativeStringTools;
import lua.Boot;
import lua.Os;
import haxe.io.Bytes;
import haxe.io.Error;
import haxe.io.Eof;

class FileInput extends haxe.io.Input {
	var f:FileHandle;
	var _eof:Bool;

	public function new(f:FileHandle){
		if (f == null) throw 'Invalid filehandle : $f';
		this.bigEndian = Boot.platformBigEndian;
		this.f = f;
		this._eof = false;
	}

	inline public function seek( p : Int, pos : FileSeek ) : Void {
		var arg = switch(pos){
			case SeekBegin : "set";
			case SeekCur   : "cur";
			case SeekEnd   : "end";
		}
		_eof = false;
		return f.seek(arg, p);
	}

	inline public function tell() : Int {
		return f.seek();
	}

	inline public function eof() : Bool {
		return _eof;
	}

	override inline public function readByte() : Int {
		var byte = f.read(1);
		if (byte == null) {
			_eof = true;
			throw new haxe.io.Eof();
		}
		return NativeStringTools.byte(byte);
	}

	override inline public function close() : Void {
		f.close();
	}
	override public function readAll( ?bufsize : Int ) : Bytes {
		if( bufsize == null )
			bufsize = (1 << 14); // 16 Ko
		var buf = Bytes.alloc(bufsize);
		var total = new haxe.io.BytesBuffer();
		try {
			while( true ) {
				var len = readBytes(buf,0,bufsize);
				if (len == 0) break;
				total.addBytes(buf,0,len);
			}
		} catch( e : Eof ) _eof = true;
		return total.getBytes();
	}


}
