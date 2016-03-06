/*
 * Copyright (C)2005-2016 Haxe Foundation
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

typedef FileHandle = hl.types.NativeAbstract<"hl_fdesc">;

@:access(String)
@:coreApi class File {

	public static function getContent( path : String ) : String {
		var size = 0;
		var bytes = file_contents(path.bytes, size);
		if( bytes == null ) throw new Sys.SysError("Can't read "+path);
		return String.fromUTF8(bytes);
	}

	public static function getBytes( path : String ) : haxe.io.Bytes {
		var size = 0;
		var bytes = file_contents(path.bytes, size);
		if( bytes == null ) throw new Sys.SysError("Can't open "+path);
		return haxe.io.Bytes.ofData(new haxe.io.BytesData(bytes,size));
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
		var f = file_open(path.bytes,(if( binary ) "rb" else "r").bytes);
		if( f == null ) throw new Sys.SysError("Can't open "+path);
		return @:privateAccess new FileInput(f);
	}

	public static function write( path : String, binary : Bool = true ) : FileOutput {
		var f = file_open(path.bytes,(if( binary ) "wb" else "w").bytes);
		if( f == null ) throw new Sys.SysError("Can't open "+path+" for writing");
		return @:privateAccess new FileOutput(f);
	}

	public static function append( path : String, binary : Bool = true ) : FileOutput {
		var f = file_open(path.bytes,(if( binary ) "ab" else "a").bytes);
		if( f == null ) throw new Sys.SysError("Can't open "+path+" for append");
		return @:privateAccess new FileOutput(f);
	}

	public static function copy( srcPath : String, dstPath : String ) : Void {
		var s = read(srcPath,true);
		var d = write(dstPath,true);
		d.writeInput(s);
		s.close();
		d.close();
	}

	@:hlNative("std", "file_open") static function file_open( path : hl.types.Bytes, access : hl.types.Bytes ) : FileHandle { return null; }
	@:hlNative("std", "file_contents") static function file_contents( path : hl.types.Bytes, size : hl.types.Ref<Int> ) : hl.types.Bytes { return null; }

}
