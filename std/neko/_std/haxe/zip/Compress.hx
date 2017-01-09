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
class Compress {

	var s : Dynamic;

	public function new( level : Int ) : Void {
		s = _deflate_init(level);
	}

	public function execute( src : haxe.io.Bytes, srcPos : Int, dst : haxe.io.Bytes, dstPos : Int ) : { done : Bool, read : Int, write : Int } {
		return _deflate_buffer(s,src.getData(),srcPos,dst.getData(),dstPos);
	}

	public function setFlushMode( f : FlushMode ) : Void {
		_set_flush_mode(s,untyped Std.string(f).__s);
	}

	public function close() : Void {
		_deflate_end(s);
	}

	public static function run( s : haxe.io.Bytes, level : Int ) : haxe.io.Bytes {
		var c = new Compress(level);
		c.setFlushMode(FlushMode.FINISH);
		var out = haxe.io.Bytes.alloc(_deflate_bound(c.s,s.length));
		var r = c.execute(s,0,out,0);
		c.close();
		if( !r.done || r.read != s.length )
			throw "Compression failed";
		return out.sub(0,r.write);
	}

	static var _deflate_init = neko.Lib.load("zlib","deflate_init",1);
	static var _deflate_bound = neko.Lib.load("zlib","deflate_bound",2);
	static var _deflate_buffer = neko.Lib.load("zlib","deflate_buffer",5);
	static var _deflate_end = neko.Lib.load("zlib","deflate_end",1);
	static var _set_flush_mode = neko.Lib.load("zlib","set_flush_mode",2);

}
