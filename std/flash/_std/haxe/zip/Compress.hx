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

	public function new( level : Int ) : Void {
		throw "Not implemented for this platform";
	}

	public function execute( src : haxe.io.Bytes, srcPos : Int, dst : haxe.io.Bytes, dstPos : Int ) : { done : Bool, read : Int, write : Int } {
		return null;
	}

	public function setFlushMode( f : FlushMode ) : Void {
	}

	public function close() : Void {
	}

	public static function run( s : haxe.io.Bytes, level : Int ) : haxe.io.Bytes {
		if( s.length == 0 ) {
			// Flash returns 0 bytes for 0 length compress (which can't be decoded on other platforms...)
			var b = haxe.io.Bytes.alloc(8);
			b.set(0,0x78);b.set(1,0xDA);b.set(2,0x03);
			b.set(7,0x01);
			return b;
		}
		var tmp = new flash.utils.ByteArray();
		tmp.writeBytes(s.getData(),0,s.length);
		tmp.compress();
		return haxe.io.Bytes.ofData(tmp);
	}

}
