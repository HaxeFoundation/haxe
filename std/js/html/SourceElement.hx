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

// This file is generated from mozilla\HTMLSourceElement.webidl. Do not edit!

package js.html;

/**
	The `HTMLSourceElement` interface provides special properties (beyond the regular `HTMLElement` object interface it also has available to it by inheritance) for manipulating `source` elements.

	Documentation [HTMLSourceElement](https://developer.mozilla.org/en-US/docs/Web/API/HTMLSourceElement) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/HTMLSourceElement$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/HTMLSourceElement>
**/
@:native("HTMLSourceElement")
extern class SourceElement extends Element
{
	
	/**
		Is a `DOMString` reflecting the `src` HTML attribute, containing the URL for the media resource. The `HTMLSourceElement.src` property has a meaning only when the associated `source` element is nested in a media element that is a `video` or an `audio` element. It has no meaning and is ignored when it is nested in a `picture` element.
	**/
	var src : String;
	
	/**
		Is a `DOMString` reflecting the `type` HTML attribute, containing the type of the media resource.
	**/
	var type : String;
	
	/**
		Is a `DOMString` reflecting the `srcset` HTML attribute, containing a list of candidate images, separated by a comma (`',', U+002C COMMA`). A candidate image is a URL followed by a `'w'` with the width of the images, or an `'x'` followed by the pixel density.
	**/
	var srcset : String;
	
	/**
		Is a `DOMString` representing image sizes between breakpoints
	**/
	var sizes : String;
	
	/**
		Is a `DOMString` reflecting the `media` HTML attribute, containing the intended type of the media resource.
	**/
	var media : String;
	
}