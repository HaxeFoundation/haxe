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

// This file is generated from mozilla\TextEncoder.webidl. Do not edit!

package js.html;

/**
	TextEncoder takes a stream of code points as input and emits a stream of bytes. For a more scalable, non-native library, see `StringView` – a C-like representation of strings based on typed arrays.

	Documentation [TextEncoder](https://developer.mozilla.org/en-US/docs/Web/API/TextEncoder) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/TextEncoder$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/TextEncoder>
**/
@:native("TextEncoder")
extern class TextEncoder
{
	
	/**
		Is a `DOMString` containing the name of the encoder, that is a string describing the method the `TextEncoder` will use.
	**/
	var encoding(default,null) : String;
	
	/** @throws DOMError */
	function new( ?utfLabel : String = "utf-8" ) : Void;
	
	/**
		Returns a `Uint8Array` containing utf-8 encoded text.
	**/
	function encode( ?input : String = "" ) : Uint8Array;
}