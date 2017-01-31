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

// This file is generated from mozilla\HTMLAreaElement.webidl. Do not edit!

package js.html;

/**
	The `HTMLAreaElement` interface provides special properties and methods (beyond those of the regular object `HTMLElement` interface it also has available to it by inheritance) for manipulating the layout and presentation of area elements.

	Documentation [HTMLAreaElement](https://developer.mozilla.org/en-US/docs/Web/API/HTMLAreaElement) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/HTMLAreaElement$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/HTMLAreaElement>
**/
@:native("HTMLAreaElement")
extern class AreaElement extends Element
{
	
	/**
		Is a `DOMString` that reflects the `alt` HTML attribute, containing alternative text for the element.
	**/
	var alt : String;
	
	/**
		Is a `DOMString` that reflects the `coords` HTML attribute, containing coordinates to define the hot-spot region.
	**/
	var coords : String;
	
	/**
		Is a `DOMString` that reflects the `shape` HTML attribute, indicating the shape of the hot-spot, limited to known values.
	**/
	var shape : String;
	
	/**
		Is a `DOMString` that reflects the `target` HTML attribute, indicating the browsing context in which to open the linked resource.
	**/
	var target : String;
	
	/**
		Is a `DOMString` indicating that the linked resource is intended to be downloaded rather than displayed in the browser. The value represent the proposed name of the file. If the name is not a valid filename of the underlying OS, browser will adapt it.
	**/
	var download : String;
	var ping : String;
	
	/**
		Is a `DOMString` that reflects the `rel` HTML attribute, indicating relationships of the current document to the linked resource.
	**/
	var rel : String;
	
	/**
		Returns a `DOMTokenList` that reflects the `rel` HTML attribute, indicating relationships of the current document to the linked resource, as a list of tokens.
	**/
	var relList(default,null) : DOMTokenList;
	
	/**
		Is a `Boolean` flag indicating if the area is inactive (`true`) or active (`false`).
	**/
	var noHref : Bool;
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