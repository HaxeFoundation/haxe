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

import cpp.NativeFile;

@:coreApi
class File {

	public static function getContent( path : String ) : String {
		return NativeFile.file_contents_string(path);
	}

	public static function getBytes( path : String ) : haxe.io.Bytes {
		var data = NativeFile.file_contents_bytes(path);
		return haxe.io.Bytes.ofData(data);
	}

	public static function saveContent( path : String, content : String ) : Void {
		var f = write(path);
		f.writeString(content);
		f.close();
	}

	public static function saveBytes( path : String, bytes : haxe.io.Bytes ) : Void {
		var f = write(path);
		f.write(bytes);
		f.close();
	}

	public static function read( path : String, binary : Bool = true ) : FileInput {
		return untyped new FileInput(NativeFile.file_open(path,(if( binary ) "rb" else "r")));
	}

	public static function write( path : String, binary : Bool = true ) : FileOutput {
		return untyped new FileOutput(NativeFile.file_open(path,(if( binary ) "wb" else "w")));
	}

	public static function append( path : String, binary : Bool = true ) : FileOutput {
		return untyped new FileOutput(NativeFile.file_open(path,(if( binary ) "ab" else "a")));
	}

	public static function copy( srcPath : String, dstPath : String ) : Void {
		var s = read(srcPath,true);
		var d = write(dstPath,true);
		d.writeInput(s);
		s.close();
		d.close();
	}

}
