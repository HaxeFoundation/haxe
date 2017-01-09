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

private typedef Inflater = hl.Abstract<"fmt_zip">;

@:coreApi @:hlNative("fmt")
class Uncompress {

	var s : Inflater;

	public function new( ?windowBits : Int ) : Void {
		s = inflate_init(windowBits);
	}

	public function execute( src : haxe.io.Bytes, srcPos : Int, dst : haxe.io.Bytes, dstPos : Int ) : { done : Bool, read : Int, write : Int } {
		var read = 0, write = 0;
		var done = inflate_buffer(s,src.getData(),srcPos,src.length,dst.getData(),dstPos,dst.length,read,write);
		return { done : done, read : read, write : write };
	}

	public function setFlushMode( f : FlushMode ) : Void {
		zip_flush_mode(s,f.getIndex());
	}

	public function close() : Void {
		zip_end(s);
	}

	public static function run( src : haxe.io.Bytes, ?bufsize : Int ) : haxe.io.Bytes {
		var u = new Uncompress(null);
		if( bufsize == null ) bufsize = 1 << 16; // 64K
		var tmp = haxe.io.Bytes.alloc(bufsize);
		var b = new haxe.io.BytesBuffer();
		var pos = 0;
		u.setFlushMode(FlushMode.SYNC);
		while( true ) {
			var r = u.execute(src,pos,tmp,0);
			b.addBytes(tmp,0,r.write);
			pos += r.read;
			if( r.done )
				break;
		}
		u.close();
		return b.getBytes();
	}

	static function inflate_init( bits : Int ) : Inflater { return null; }
	static function inflate_buffer( i : Inflater, bytes : hl.Bytes, bytesPos : Int, bytesLen : Int, dst : hl.Bytes, dstPos : Int, dstLen : Int, read : hl.Ref<Int>, write : hl.Ref<Int>) : Bool {
		return false;
	}
	static function zip_end( i : Inflater ) : Void { }
	static function zip_flush_mode( i : Inflater, flush : Int ) : Void {}

}
