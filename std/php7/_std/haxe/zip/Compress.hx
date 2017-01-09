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

	private var level : Int;

	public function new( level : Int ) : Void {
		this.level = level;
	}

	public function execute( src : haxe.io.Bytes, srcPos : Int, dst : haxe.io.Bytes, dstPos : Int ) : { done : Bool, read : Int, write : Int } {
		var input = src.sub( srcPos , src.length - srcPos );
		var data = run( input , level );
		dst.blit( dstPos , data , 0 , data.length );

		return {
			done: true,
			read: input.length,
			write: data.length
		};
	}

	public function setFlushMode( f : FlushMode ) : Void {
	}

	public function close() : Void {
	}

	public static function run( s : haxe.io.Bytes, level : Int ) : haxe.io.Bytes {
		var c = untyped __call__("gzcompress", s.toString(), level);
		return haxe.io.Bytes.ofString(c);
	}
}
