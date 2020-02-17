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

@:coreApi @:buildXml('<include name="${HXCPP}/src/hx/libs/zlib/Build.xml" />')
class Compress {
	var s:Dynamic;

	public function new(level:Int):Void {
		s = _deflate_init(level);
	}

	public function execute(src:haxe.io.Bytes, srcPos:Int, dst:haxe.io.Bytes, dstPos:Int):{done:Bool, read:Int, write:Int} {
		return _deflate_buffer(s, src.getData(), srcPos, dst.getData(), dstPos);
	}

	public function setFlushMode(f:FlushMode):Void {
		_set_flush_mode(s, Std.string(f));
	}

	public function close():Void {
		_deflate_end(s);
	}

	public static function run(s:haxe.io.Bytes, level:Int):haxe.io.Bytes {
		var c = new Compress(level);
		c.setFlushMode(FlushMode.FINISH);
		var out = haxe.io.Bytes.alloc(_deflate_bound(c.s, s.length));
		var r = c.execute(s, 0, out, 0);
		c.close();
		if (!r.done || r.read != s.length)
			throw "Compression failed";
		return out.sub(0, r.write);
	}

	@:native("_hx_deflate_init")
	extern static function _deflate_init(level:Int):Dynamic;

	@:native("_hx_deflate_bound")
	extern static function _deflate_bound(handle:Dynamic, length:Int):Int;

	@:native("_hx_deflate_buffer")
	extern static function _deflate_buffer(handle:Dynamic, src:haxe.io.BytesData, srcPos:Int, dest:haxe.io.BytesData,
		destPos:Int):{done:Bool, read:Int, write:Int};

	@:native("_hx_deflate_end")
	extern static function _deflate_end(handle:Dynamic):Void;

	@:native("_hx_zip_set_flush_mode")
	extern static function _set_flush_mode(handle:Dynamic, flushMode:String):Void;
}
