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

// This file is generated from mozilla\FontFace.webidl. Do not edit!

package js.html;

import js.lib.Promise;

/**
	The `FontFace` interface represents a single usable font face. It allows control of the source of the font face, being a URL to an external resource, or a buffer; it also allows control of when the font face is loaded and its current status.

	Documentation [FontFace](https://developer.mozilla.org/en-US/docs/Web/API/FontFace) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/FontFace$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/FontFace>
**/
@:native("FontFace")
extern class FontFace {

	/**
		Is a `CSSOMString` that contains the family of the font. It is equivalent to the `@font-face/font-family` descriptor.
	**/
	var family : String;

	/**
		Is a `CSSOMString` that contains the style of the font. It is equivalent to the `@font-face/font-style` descriptor.
	**/
	var style : String;

	/**
		Is a `CSSOMString` that contains the weight of the font. It is equivalent to the `@font-face/font-weight` descriptor.
	**/
	var weight : String;

	/**
		Is a `CSSOMString` that contains how the font stretches. It is equivalent to the `@font-face/font-stretch` descriptor.
	**/
	var stretch : String;

	/**
		Is a `CSSOMString` that contains the range of code encompassed the font. It is equivalent to the `@font-face/unicode-range` descriptor.
	**/
	var unicodeRange : String;

	/**
		Is a `CSSOMString` that contains the variant of the font. It is equivalent to the `@font-face/font-variant` descriptor.
	**/
	var variant : String;

	/**
		Is a `CSSOMString` that contains the features of the font. It is equivalent to the `@font-face/font-feature-settings`descriptor.
	**/
	var featureSettings : String;

	/**
		Returns an enumerated value indicating the status of the font. It can be one of the following: `"unloaded"`, `"loading"`, `"loaded"`, or `"error"`.
	**/
	var status(default,null) : FontFaceLoadStatus;

	/**
		Returns a `Promise` to a `FontFace` that fulfills when the font is completely loaded and rejects when an error happens.
	**/
	var loaded(default,null) : Promise<FontFace>;

	/** @throws DOMError */
	@:overload( function( family : String, source : js.lib.ArrayBuffer, ?descriptors : FontFaceDescriptors) : FontFace {} )
	@:overload( function( family : String, source : js.lib.ArrayBufferView, ?descriptors : FontFaceDescriptors) : FontFace {} )
	function new( family : String, source : String, ?descriptors : FontFaceDescriptors ) : Void;

	/**
		Loads the font, returning a `Promise` to a `FontFace` that fulfills when the font is completely loaded and rejects when an error happens.
		@throws DOMError
	**/
	function load() : Promise<FontFace>;
}