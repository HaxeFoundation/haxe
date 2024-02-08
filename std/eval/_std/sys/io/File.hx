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

package sys.io;

@:coreApi
class File {
	extern static public function getContent(path:String):String;

	extern static public function saveContent(path:String, content:String):Void;

	extern static public function getBytes(path:String):haxe.io.Bytes;

	extern static public function saveBytes(path:String, bytes:haxe.io.Bytes):Void;

	extern static public function read(path:String, binary:Bool = true):FileInput;

	extern static public function write(path:String, binary:Bool = true):FileOutput;

	extern static public function append(path:String, binary:Bool = true):FileOutput;

	extern static public function update(path:String, binary:Bool = true):FileOutput;

	static public function copy(srcPath:String, dstPath:String):Void {
		var s = read(srcPath, true);
		var d = write(dstPath, true);
		d.writeInput(s);
		s.close();
		d.close();
	}
}
