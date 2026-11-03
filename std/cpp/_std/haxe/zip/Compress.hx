/*
 * Copyright (C)2005-2019 Haxe Foundation
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

import haxe.io.Bytes;
using cpp.marshal.ViewExtensions;

@:coreApi class Compress {
	private final impl : cpp.zip.Compress;

	public function new(level:Int) {
		impl = cpp.zip.Compress.create(level);
	}

	public function execute(src:haxe.io.Bytes, srcPos:Int, dst:haxe.io.Bytes, dstPos:Int):{done:Bool, read:Int, write:Int} {
		final srcView = src.asView().slice(srcPos);
		final dstView = dst.asView().slice(dstPos);
		final result  = impl.execute(srcView, dstView);

		return { write : result.write, read : result.read, done : result.done }
	}

	public function setFlushMode(f:FlushMode):Void {
		impl.setFlushMode(switch f {
			case NO: cpp.zip.Flush.None;
			case SYNC: cpp.zip.Flush.Sync;
			case FULL: cpp.zip.Flush.Full;
			case FINISH: cpp.zip.Flush.Finish;
			case BLOCK: cpp.zip.Flush.Block;
		});
	}

	public function close():Void {
		impl.close();
	}

	public static function run(s:haxe.io.Bytes, level:Int):haxe.io.Bytes {
		return Bytes.ofData(cpp.zip.Compress.run(s.asView(), level));
	}
}