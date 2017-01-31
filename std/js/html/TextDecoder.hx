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

// This file is generated from mozilla\TextDecoder.webidl. Do not edit!

package js.html;

/**
	The `TextDecoder` interface represents a decoder for a specific method, that is a specific character encoding, like `utf-8`, `iso-8859-2`, `koi8`, `cp1261`, `gbk`, ... A decoder takes a stream of bytes as input and emits a stream of code points. For a more scalable, non-native library, see `StringView` – a C-like representation of strings based on typed arrays.

	Documentation [TextDecoder](https://developer.mozilla.org/en-US/docs/Web/API/TextDecoder) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/TextDecoder$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/TextDecoder>
**/
@:native("TextDecoder")
extern class TextDecoder
{
	
	/**
		Is a `DOMString` containing the name of the decoder, that is a string describing the method the `TextDecoder` will use.
	**/
	var encoding(default,null) : String;
	
	/**
		Is a `Boolean` indicating whether the error mode is fatal.
	**/
	var fatal(default,null) : Bool;
	
	/** @throws DOMError */
	function new( ?label : String = "utf-8", ?options : TextDecoderOptions ) : Void;
	/** @throws DOMError */
	
	/**
		Returns a `DOMString` containing the text decoded with the method of the specific `TextDecoder` object.
	**/
	function decode( ?input : haxe.extern.EitherType<ArrayBufferView,ArrayBuffer>, ?options : TextDecodeOptions ) : String;
}