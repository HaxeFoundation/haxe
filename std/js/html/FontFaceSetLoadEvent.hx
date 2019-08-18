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

// This file is generated from mozilla\FontFaceSetLoadEvent.webidl. Do not edit!

package js.html;

/**
	The `FontFaceSetLoadEvent` interface of the the Css Font Loading API is fired whenever a `FontFaceSet` loads.

	Documentation [FontFaceSetLoadEvent](https://developer.mozilla.org/en-US/docs/Web/API/FontFaceSetLoadEvent) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/FontFaceSetLoadEvent$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/FontFaceSetLoadEvent>
**/
@:native("FontFaceSetLoadEvent")
extern class FontFaceSetLoadEvent extends Event {
	
	/**
		Returns an array of `FontFace` instances each of which represents a single usable font.
	**/
	var fontfaces(default,null) : Array<FontFace>;
	
	/** @throws DOMError */
	function new( type : String, ?eventInitDict : FontFaceSetLoadEventInit ) : Void;
}