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

import haxe.extern.EitherType;
#if haxe4
import js.lib.ArrayBuffer;
import js.lib.ArrayBufferView;
#else
import js.html.ArrayBuffer;
import js.html.ArrayBufferView;
#end

/**
	An implementation of the WHATWG Encoding Standard `TextDecoder` API.

	@see https://nodejs.org/api/util.html#util_class_util_textdecoder
**/
@:jsRequire("util", "TextDecoder")
extern class TextDecoder {
	/**
		Creates an new `TextDecoder` instance.

		@see https://nodejs.org/api/util.html#util_new_textdecoder_encoding_options
	**/
	function new(?encoding:String, ?options:TextDecoderOptions);

	/**
		Decodes the `input` and returns a string.

		@see https://nodejs.org/api/util.html#util_textdecoder_decode_input_options
	**/
	function decode(?input:EitherType<ArrayBuffer, ArrayBufferView>, ?options:TextDecodeOptions):String;

	/**
		The encoding supported by the `TextDecoder` instance.

		@see https://nodejs.org/api/util.html#util_textdecoder_encoding
	**/
	var encoding(default, null):String;

	/**
		The value will be `true` if decoding errors result in a `TypeError` being thrown.

		@see https://nodejs.org/api/util.html#util_textdecoder_fatal
	**/
	var fatal(default, null):Bool;

	/**
		The value will be `true` if the decoding result will include the byte order mark.

		@see https://nodejs.org/api/util.html#util_textdecoder_ignorebom
	**/
	var ignoreBOM(default, null):Bool;
}

/**
	Options object used by `new TextDecoder()`.

	@see https://nodejs.org/api/util.html#util_new_textdecoder_encoding_options
**/
typedef TextDecoderOptions = {
	/**
		`true` if decoding failures are fatal.
		This option is only supported when ICU is enabled (see Internationalization).

		Default: `false`.
	**/
	@:optional var fatal:Bool;

	/**
		When `true`, the `TextDecoder` will include the byte order mark in the decoded result.
		When `false`, the byte order mark will be removed from the output.
		This option is only used when `encoding` is `'utf-8'`, `'utf-16be'` or `'utf-16le'`.

		Default: `false`.
	**/
	@:optional var ignoreBOM:Bool;
}

/**
	Options object used by `TextDecoder.decode`.
**/
typedef TextDecodeOptions = {
	/**
		`true` if additional chunks of data are expected.

		Default: `false`.
	**/
	@:optional var stream:Bool;
}
