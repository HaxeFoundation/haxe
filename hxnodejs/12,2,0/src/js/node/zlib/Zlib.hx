/*
 * Copyright (C)2014-2020 Haxe Foundation
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

package js.node.zlib;

/**
	Not exported by the zlib module.
	It is documented here because it is the base class of the compressor/decompressor classes.
**/
extern class Zlib extends js.node.stream.Transform<Zlib> {
	/**
		Flush pending data.

		`kind` defaults to `Zlib.Z_FULL_FLUSH`.

		Don't call this frivolously, premature flushes negatively impact the effectiveness of the compression algorithm.
	**/
	@:overload(function(kind:Int, callback:Void->Void):Void {})
	function flush(callback:Void->Void):Void;

	/**
		Dynamically update the compression level and compression strategy.
		Only applicable to deflate algorithm.
	**/
	function params(level:Int, strategy:Int, callback:Void->Void):Void;

	/**
		Reset the compressor/decompressor to factory defaults.
		Only applicable to the inflate and deflate algorithms.
	**/
	function reset():Void;
}
