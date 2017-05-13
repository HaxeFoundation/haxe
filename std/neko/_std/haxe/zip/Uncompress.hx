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

@:coreApi
class Uncompress {

	var s : Dynamic;

	public function new( ?windowBits : Int ) : Void {
		s = _inflate_init(windowBits);
	}

	public function execute( src : haxe.io.Bytes, srcPos : Int, dst : haxe.io.Bytes, dstPos : Int ) : { done : Bool, read : Int, write : Int } {
		return _inflate_buffer(s,src.getData(),srcPos,dst.getData(),dstPos);
	}

	public function setFlushMode( f : FlushMode ) : Void {
		_set_flush_mode(s,untyped Std.string(f).__s);
	}

	public function close() : Void {
		_inflate_end(s);
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

	static var _inflate_init = neko.Lib.load("zlib","inflate_init",1);
	static var _inflate_buffer = neko.Lib.load("zlib","inflate_buffer",5);
	static var _inflate_end = neko.Lib.load("zlib","inflate_end",1);
	static var _set_flush_mode = neko.Lib.load("zlib","set_flush_mode",2);

}
