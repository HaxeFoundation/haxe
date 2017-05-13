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

class Uncompress
{
	public function new( ?windowBits : Int ) {
		throw "Not implemented for this platform";
	}

	public function execute( src : haxe.io.Bytes, srcPos : Int, dst : haxe.io.Bytes, dstPos : Int ) : { done : Bool, read : Int, write : Int } {
		return null;
	}

	public function setFlushMode( f : FlushMode ) {
	}

	public function close() {
	}

	public static function run( src : haxe.io.Bytes, ?bufsize : Int ) : haxe.io.Bytes
	{
		var decompresser = new java.util.zip.Inflater();
		var buf = haxe.io.Bytes.alloc(bufsize == null ? src.length : bufsize).getData();

		var out = new java.io.ByteArrayOutputStream(src.length);
		decompresser.setInput(src.getData(), 0, src.length);

		while (!decompresser.finished())
		{
			var count = decompresser.inflate(buf);
			out.write(buf, 0, count);
		}
		out.close();
		return haxe.io.Bytes.ofData(out.toByteArray());
	}

}

