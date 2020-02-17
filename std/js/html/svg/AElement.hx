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

// This file is generated from mozilla\SVGAElement.webidl. Do not edit!

package js.html.svg;

/**
	The `SVGAElement` interface provides access to the properties of `a` element, as well as methods to manipulate them.

	Documentation [SVGAElement](https://developer.mozilla.org/en-US/docs/Web/API/SVGAElement) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/SVGAElement$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/SVGAElement>
**/
@:native("SVGAElement")
extern class AElement extends GraphicsElement {
	
	/**
		It corresponds to the `target` attribute of the given element.
	**/
	var target(default,null) : AnimatedString;
	
	/**
		See `HTMLAnchorElement.download`.
	**/
	var download : String;
	
	/**
		Is a `DOMString` that reflects the ping attribute, containing a space-separated list of URLs to which, when the hyperlink is followed, `POST` requests with the body `PING` will be sent by the browser (in the background). Typically used for tracking.
	**/
	var ping : String;
	
	/**
		See `HTMLAnchorElement.rel`.
	**/
	var rel : String;
	
	/**
		See `HTMLAnchorElement.referrerPolicy`.
	**/
	var referrerPolicy : String;
	
	/**
		See `HTMLAnchorElement.relList`.
	**/
	var relList(default,null) : js.html.DOMTokenList;
	
	/**
		Is a `DOMString` that reflects the `hreflang` attribute, indicating the language of the linked resource.
	**/
	var hreflang : String;
	
	/**
		Is a `DOMString` that reflects the `type` attribute, indicating the MIME type of the linked resource.
	**/
	var type : String;
	
	/**
		Is a `DOMString` being a synonym for the `Node.textContent` property.
	**/
	var text : String;
	
	/**
		See `HTMLAnchorElement.href`.
	**/
	var href(default,null) : AnimatedString;
	
}