/*
 * Copyright (C)2005-2019 Haxe Foundation
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

package haxe.zip;

class Tools {
	public static function compress(f:Entry, level:Int) {
		if (f.compressed)
			return;
		// this should be optimized with a temp buffer
		// that would discard the first two bytes
		// (in order to prevent 2x mem usage for large files)
		var data = haxe.zip.Compress.run(f.data, level);
		f.compressed = true;
		f.data = data.sub(2, data.length - 6);
		f.dataSize = f.data.length;
	}

	public static function uncompress(f:Entry) {
		if( !f.compressed )
			return;

		var c = new Uncompress(-15);
		var s = haxe.io.Bytes.alloc(f.fileSize);
		var r = c.execute(f.data,0,s,0);
		c.close();
		if( !r.done || r.read != f.data.length || r.write != f.fileSize )
			throw "Invalid compressed data for "+f.fileName;
		f.compressed = false;
		f.dataSize = f.fileSize;
		f.data = s;
	}
}
