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
package haxe.zip;

private typedef Deflater = hl.Abstract<"fmt_zip">;

@:coreApi @:hlNative("fmt")
class Compress {

	var s : Deflater;

	public function new( level : Int ) : Void {
		s = deflate_init(level);
	}

	public function execute( src : haxe.io.Bytes, srcPos : Int, dst : haxe.io.Bytes, dstPos : Int ) : { done : Bool, read : Int, write : Int } {
		var read = 0, write = 0;
		var done = deflate_buffer(s,src.getData(),srcPos,src.length,dst.getData(),dstPos,dst.length,read,write);
		return { done : done, read : read, write : write };
	}

	public function setFlushMode( f : FlushMode ) : Void {
		@:privateAccess Uncompress.zip_flush_mode(cast s,f.getIndex());
	}

	public function close() : Void {
		@:privateAccess Uncompress.zip_end(cast s);
	}

	public static function run( s : haxe.io.Bytes, level : Int ) : haxe.io.Bytes {
		var c = new Compress(level);
		c.setFlushMode(FlushMode.FINISH);
		var out = haxe.io.Bytes.alloc(deflate_bound(c.s,s.length));
		var r = c.execute(s,0,out,0);
		c.close();
		if( !r.done || r.read != s.length )
			throw "Compression failed";
		if( r.write < out.length*0.66 )
			return out.sub(0, r.write);
		@:privateAccess out.length = r.write;
		return out;
	}

	static function deflate_init( level : Int ) : Deflater { return null; }
	static function deflate_buffer( i : Deflater, bytes : hl.Bytes, bytesPos : Int, bytesLen : Int, dst : hl.Bytes, dstPos : Int, dstLen : Int, read : hl.Ref<Int>, write : hl.Ref<Int>) : Bool {
		return false;
	}
	static function deflate_bound( i : Deflater, length : Int ) : Int { return 0; }

}
