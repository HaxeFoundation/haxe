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
class File {

	public static function getContent( path : String ) : String
	{
		var f = read(path, false);
		var ret = f.readAll().toString();
		f.close();
		return ret;
	}

	public static function saveContent( path : String, content : String ) : Void
	{
		var f = write(path, false);
		f.writeString(content);
		f.close();
	}

	public static function getBytes( path : String ) : haxe.io.Bytes
	{
		var f = read(path, true);
		var ret = f.readAll();
		f.close();
		return ret;
	}

	public static function saveBytes( path : String, bytes : haxe.io.Bytes ) : Void
	{
		var f = write(path, true);
		f.writeBytes(bytes, 0, bytes.length);
		f.close();
	}

	public static function read( path : String, binary : Bool = true ) : FileInput
	{
		#if std_buffer //standardize 4kb buffers
		var stream = new cs.system.io.FileStream(path, Open, Read, ReadWrite, 4096);
		#else
		var stream = new cs.system.io.FileStream(path, Open, Read, ReadWrite);
		#end
		return new FileInput(stream);
	}

	public static function write( path : String, binary : Bool = true ) : FileOutput
	{
		#if std_buffer //standardize 4kb buffers
		var stream = new cs.system.io.FileStream(path, Create, Write, ReadWrite, 4096);
		#else
		var stream = new cs.system.io.FileStream(path, Create, Write, ReadWrite);
		#end
		return new FileOutput(stream);
	}

	public static function append( path : String, binary : Bool = true ) : FileOutput
	{
		#if std_buffer //standardize 4kb buffers
		var stream = new cs.system.io.FileStream(path, Append, Write, ReadWrite, 4096);
		#else
		var stream = new cs.system.io.FileStream(path, Append, Write, ReadWrite);
		#end
		return new FileOutput(stream);
	}

	public static function copy( srcPath : String, dstPath : String ) : Void
	{
		cs.system.io.File.Copy(srcPath, dstPath);
	}
}
