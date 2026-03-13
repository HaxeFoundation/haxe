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

package js.node.util;

#if haxe4
import js.lib.Uint8Array;
#else
import js.html.Uint8Array;
#end

/**
	An implementation of the WHATWG Encoding Standard `TextEncoder` API.

	@see https://nodejs.org/api/util.html#util_class_util_textencoder
**/
@:jsRequire("util", "TextEncoder")
extern class TextEncoder {
	function new();

	/**
		UTF-8 encodes the `input` string and returns a `Uint8Array` containing the encoded bytes.

		@see https://nodejs.org/api/util.html#util_textencoder_encode_input
	**/
	function encode(?input:String):Uint8Array;

	/**
		The encoding supported by the `TextEncoder` instance.

		@see https://nodejs.org/api/util.html#util_textencoder_encoding
	**/
	var encoding(default, null):String;
}
