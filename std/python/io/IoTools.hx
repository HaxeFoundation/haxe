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
package python.io;

import python.io.FileBytesInput;
import python.io.FileTextInput;
import python.io.FileTextOutput;
import python.io.FileBytesOutput;
import python.lib.io.RawIOBase;
import python.lib.io.TextIOBase;
import sys.io.FileInput;
import sys.io.FileOutput;

import python.lib.io.IOBase.SeekSet;


class IoTools {

	public static function createFileInputFromText (t:TextIOBase) {
		return new FileInput(new FileTextInput(t));
	}

	public static function createFileInputFromBytes (t:RawIOBase) {
		return new FileInput(new FileBytesInput(t));
	}

	public static function createFileOutputFromText (t:TextIOBase) {
		return new FileOutput(new FileTextOutput(t));
	}

	public static function createFileOutputFromBytes (t:RawIOBase) {
		return new FileOutput(new FileBytesOutput(t));
	}

	public static function seekInTextMode (stream:TextIOBase, tell:Void->Int , p : Int, pos : sys.io.FileSeek)
	{
 		var pos = switch (pos) {
 			case SeekBegin:
 				SeekSet;
 			case SeekCur:
 				p = tell() + p;
 				SeekSet;
 			case SeekEnd :
 				stream.seek(0, SeekEnd);
 				p = tell() + p;
 				SeekSet;
 		}

 		stream.seek(p, pos);
	}

	public static function seekInBinaryMode (stream:RawIOBase, p : Int, pos : sys.io.FileSeek)
	{
 		var pos = switch(pos)
		{
			case SeekBegin: SeekSet;
			case SeekCur: SeekCur;
			case SeekEnd: SeekEnd;
		};
		stream.seek(p, pos);
	}

}