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

import python.io.IoTools;
import sys.io.FileInput;

@:coreApi
class File {

	public static function getContent( path : String ) : String
	{
		var f:python.lib.io.TextIOBase = cast python.lib.Builtins.open(path, "r", -1, "utf-8", null, "");
		var content = f.read(-1);
		f.close();
		return content;
	}

	public static function saveContent( path : String, content : String ) : Void {
		var f:python.lib.io.TextIOBase = cast python.lib.Builtins.open(path, "w", -1, "utf-8", null, "");
		f.write(content);
		f.close();
	}

	public static function getBytes( path : String ) : haxe.io.Bytes {
		var f:python.lib.io.RawIOBase = cast python.lib.Builtins.open(path, "rb", -1);
		var size = f.read(-1);
		var b = haxe.io.Bytes.ofData(size);
		f.close();
		return b;
	}

	public static function saveBytes( path : String, bytes : haxe.io.Bytes ) : Void {
		var f:python.lib.io.RawIOBase = cast python.lib.Builtins.open(path, "wb", -1);
		f.write(bytes.getData());
		f.close();
	}

	public static function read( path : String, binary : Bool = true ) : FileInput {
		var mode = if (binary) "rb" else "r";

		var f = python.lib.Builtins.open(path, mode, -1, null, null, binary ? null : "");

		return if (binary) IoTools.createFileInputFromBytes(cast f) else IoTools.createFileInputFromText(cast f);
	}

	public static function write( path : String, binary : Bool = true ) : FileOutput {
		var mode = if (binary) "wb" else "w";
		var f = python.lib.Builtins.open(path, mode, -1, null, null, binary ? null : "");

		return if (binary) IoTools.createFileOutputFromBytes(cast f) else IoTools.createFileOutputFromText(cast f);
	}

	public static function append( path : String, binary : Bool = true ) : FileOutput {
		var mode = if (binary) "ab" else "a";
		var f = python.lib.Builtins.open(path, mode, -1, null, null, binary ? null : "");

		return if (binary) IoTools.createFileOutputFromBytes(cast f) else IoTools.createFileOutputFromText(cast f);
	}

	public static function copy( srcPath : String, dstPath : String ) : Void
	{
		return python.lib.Shutil.copy(srcPath, dstPath);
	}
}
