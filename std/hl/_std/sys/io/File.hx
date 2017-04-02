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

#if doc_gen
enum FileHandle { }
#else
typedef FileHandle = hl.Abstract<"hl_fdesc">;
#end

@:access(Sys)
@:coreApi class File {

	public static function getContent( path : String ) : String {
		var bytes = file_contents(Sys.getPath(path), null);
		if( bytes == null ) throw new Sys.SysError("Can't read "+path);
		return @:privateAccess String.fromUTF8(bytes);
	}

	public static function getBytes( path : String ) : haxe.io.Bytes {
		var size = 0;
		var bytes = file_contents(Sys.getPath(path), size);
		if( bytes == null ) throw new Sys.SysError("Can't read "+path);
		return @:privateAccess new haxe.io.Bytes(bytes, size);
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
		var f = file_open(Sys.getPath(path),0,binary);
		if( f == null ) throw new Sys.SysError("Can't open "+path);
		return @:privateAccess new FileInput(f);
	}

	public static function write( path : String, binary : Bool = true ) : FileOutput {
		var f = file_open(Sys.getPath(path),1,binary);
		if( f == null ) throw new Sys.SysError("Can't open "+path+" for writing");
		return @:privateAccess new FileOutput(f);
	}

	public static function append( path : String, binary : Bool = true ) : FileOutput {
		var f = file_open(Sys.getPath(path),2,binary);
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

	@:hlNative("std", "file_open") static function file_open( path : hl.Bytes, mode : Int, binary : Bool ) : FileHandle { return null; }
	@:hlNative("std", "file_contents") static function file_contents( path : hl.Bytes, size : hl.Ref<Int> ) : hl.Bytes { return null; }

}
