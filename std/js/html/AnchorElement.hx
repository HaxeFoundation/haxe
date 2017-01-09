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

// This file is generated from mozilla\HTMLAnchorElement.webidl. Do not edit!

package js.html;

/**
	The `HTMLAnchorElement` interface represents hyperlink elements and provides special properties and methods (beyond those of the regular `HTMLElement` object interface they also have available to them by inheritance) for manipulating the layout and presentation of such elements.

	Documentation [HTMLAnchorElement](https://developer.mozilla.org/en-US/docs/Web/API/HTMLAnchorElement) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/HTMLAnchorElement$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/HTMLAnchorElement>
**/
@:native("HTMLAnchorElement")
extern class AnchorElement extends Element
{
	
	/**
		Is a `DOMString` that reflects the `target` HTML attribute, indicating where to display the linked resource.
	**/
	var target : String;
	
	/**
		Is a `DOMString` indicating that the linked resource is intended to be downloaded rather than displayed in the browser. The value represent the proposed name of the file. If the name is not a valid filename of the underlying OS, browser will adapt it. The value is a URL with a scheme like `http:`, `file:`, `data:` or even `blob:` (created with `URL.createObjectURL`).
	**/
	var download : String;
	var ping : String;
	
	/**
		Is a `DOMString` that reflects the `rel` HTML attribute, specifying the relationship of the target object to the linked object.
	**/
	var rel : String;
	
	/**
		Returns a `DOMTokenList` that reflects the `rel` HTML attribute, as a list of tokens.
	**/
	var relList(default,null) : DOMTokenList;
	
	/**
		Is a `DOMString` that reflects the `hreflang` HTML attribute, indicating the language of the linked resource.
	**/
	var hreflang : String;
	
	/**
		Is a `DOMString` that reflects the `type` HTML attribute, indicating the MIME type of the linked resource.
	**/
	var type : String;
	
	/**
		Is a `DOMString` being a synonym for the `Node.textContent` property.
	**/
	var text : String;
	
	/**
		Is a `DOMString` representing a comma-separated list of coordinates.
	**/
	var coords : String;
	
	/**
		Is a `DOMString` representing the character encoding of the linked resource.
	**/
	var charset : String;
	
	/**
		Is a `DOMString` representing the anchor name.
	**/
	var name : String;
	
	/**
		Is a `DOMString` representing that the `rev` HTML attribute, specifying the relationship of the link object to the target object.
	**/
	var rev : String;
	
	/**
		Is a `DOMString` representing the shape of the active area.
	**/
	var shape : String;
	var href : String;
	var origin(default,null) : String;
	var protocol : String;
	var username : String;
	var password : String;
	var host : String;
	var hostname : String;
	var port : String;
	var pathname : String;
	var search : String;
	var hash : String;
	
}