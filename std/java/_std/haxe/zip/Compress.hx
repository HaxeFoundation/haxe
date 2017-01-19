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
import java.util.zip.Deflater;

class Compress
{
	var deflater:Deflater;
	var mode:Int;
	var finish:Bool = false;

	public function new( level : Int )
	{
		throw "Not implemented for this platform"; //FIXME: Add unit tests for Compress/Uncompress and check current implementation
		this.deflater = new Deflater(level);
		this.mode = Deflater.NO_FLUSH;
	}

	public function execute( src : haxe.io.Bytes, srcPos : Int, dst : haxe.io.Bytes, dstPos : Int ) : { done : Bool, read : Int, write : Int }
	{
		deflater.setInput(src.getData(), srcPos, src.length - srcPos);
		if (finish)
			deflater.finish();
		finish = false;

		var written = deflater.deflate(dst.getData(), dstPos, dst.length - dstPos);
		var read = deflater.getTotalIn();
		return { done: deflater.finished(), read: read, write: written };
	}

	public function setFlushMode( f : FlushMode )
	{
		this.mode = switch (f) {
			case NO:
				Deflater.NO_FLUSH;
			case SYNC:
				Deflater.SYNC_FLUSH;
			case FULL:
				Deflater.FULL_FLUSH;
			case FINISH:
				this.finish = true;
				Deflater.FULL_FLUSH;
			case BLOCK:
				throw "Not Implemented";
		}
	}

	public function close()
	{
		deflater.end();
	}

	public static function run( s : haxe.io.Bytes, level : Int ) : haxe.io.Bytes
	{
		var deflater = new java.util.zip.Deflater(level);
		deflater.setInput(s.getData());
		var outputStream = new java.io.ByteArrayOutputStream(s.length);
		deflater.finish();
		var buffer = haxe.io.Bytes.alloc(1024).getData();
		while (!deflater.finished()) {
			var count = deflater.deflate(buffer);
			outputStream.write(buffer, 0, count);
		}
		outputStream.close();
		return haxe.io.Bytes.ofData(outputStream.toByteArray());
	}
}
