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

package js.node;

#if haxe4
import js.lib.ArrayBufferView;
#else
import js.html.ArrayBufferView;
#end

/**
	The `string_decoder` module provides an API for decoding `Buffer` objects into strings in a manner that preserves
	encoded multi-byte UTF-8 and UTF-16 characters.

	@see https://nodejs.org/api/string_decoder.html#string_decoder_string_decoder
**/
@:jsRequire("string_decoder", "StringDecoder")
extern class StringDecoder {
	/**
		Creates a new `StringDecoder` instance.

		@see https://nodejs.org/api/string_decoder.html#string_decoder_new_stringdecoder_encoding
	**/
	function new(?encoding:String);

	/**
		Returns any remaining input stored in the internal buffer as a string.
		Bytes representing incomplete UTF-8 and UTF-16 characters will be replaced
		with substitution characters appropriate for the character encoding.

		@see https://nodejs.org/api/string_decoder.html#string_decoder_stringdecoder_end_buffer
	**/
	@:overload(function(buffer:Buffer):String {})
	@:overload(function(buffer:ArrayBufferView):String {})
	function end():String;

	/**
		Returns a decoded string, ensuring that any incomplete multibyte characters at the end of the `Buffer`, or
		`TypedArray`, or `DataViewor` are omitted from the returned string and stored in an internal buffer for the next
		call to `stringDecoder.write()` or `stringDecoder.end()`.

		@see https://nodejs.org/api/string_decoder.html#string_decoder_stringdecoder_write_buffer
	**/
	@:overload(function(buffer:ArrayBufferView):String {})
	function write(buffer:Buffer):String;
}
