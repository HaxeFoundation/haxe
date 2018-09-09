/*
 * Copyright (C)2005-2018 Haxe Foundation
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

@:coreApi @:buildXml('<include name="${HXCPP}/src/hx/libs/zlib/Build.xml"/>')
class Uncompress {
	var s : Dynamic;

	public function new( ?windowBits : Int ) : Void {
		s = _inflate_init(windowBits);
	}

	public function execute( src : haxe.io.Bytes, srcPos : Int, dst : haxe.io.Bytes, dstPos : Int ) : { done : Bool, read : Int, write : Int } {
		return _inflate_buffer(s,src.getData(),srcPos,dst.getData(),dstPos);
	}

	public function setFlushMode( f : FlushMode ) : Void {
		_set_flush_mode(s,untyped f.__Tag());
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

   @:native("_hx_inflate_init")
	extern static function _inflate_init(windowBits:Dynamic) : Dynamic;

   @:native("_hx_inflate_buffer")
	extern static function _inflate_buffer(handle:Dynamic, src:haxe.io.BytesData, srcPos:Int,  dest:haxe.io.BytesData, destPos:Int) : { done : Bool, read : Int, write : Int };

   @:native("_hx_inflate_end")
	extern static function _inflate_end(handle:Dynamic):Void;

   @:native("_hx_zip_set_flush_mode")
	extern static function _set_flush_mode(handle:Dynamic, flushMode:String):Void;

}
