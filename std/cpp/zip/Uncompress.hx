/*
 * Copyright (c) 2005, The haXe Project Contributors
 * All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   - Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   - Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE HAXE PROJECT CONTRIBUTORS "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE HAXE PROJECT CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 */
package cpp.zip;

class Uncompress {
	var s : Dynamic;

	public function new( windowBits : Null<Int> ) {
		s = _inflate_init(windowBits);
	}

	public function this_run( src : haxe.io.Bytes, srcPos : Int, dst : haxe.io.Bytes, dstPos : Int ) : { done : Bool, read : Int, write : Int } {
		return _inflate_buffer(s,src.getData(),srcPos,dst.getData(),dstPos);
	}

	public function setFlushMode( f : Flush ) {
		_set_flush_mode(s,untyped f.__Tag());
	}

	public function close() {
		_inflate_end(s);
	}

	public static function run( src : haxe.io.Bytes, ?bufsize ) : haxe.io.Bytes {
		var u = new Uncompress(null);
		if( bufsize == null ) bufsize = 1 << 16; // 64K
		var tmp = haxe.io.Bytes.alloc(bufsize);
		var b = new haxe.io.BytesBuffer();
		var pos = 0;
		u.setFlushMode(Flush.SYNC);
		while( true ) {
			var r = u.this_run(src,pos,tmp,0);
			b.addBytes(tmp,0,r.write);
			pos += r.read;
			if( r.done )
				break;
		}
		u.close();
		return b.getBytes();
	}

	static var _inflate_init = cpp.Lib.load("zlib","inflate_init",1);
	static var _inflate_buffer = cpp.Lib.load("zlib","inflate_buffer",5);
	static var _inflate_end = cpp.Lib.load("zlib","inflate_end",1);
	static var _set_flush_mode = cpp.Lib.load("zlib","set_flush_mode",2);

}
