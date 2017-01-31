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

// This file is generated from mozilla\HTMLEmbedElement.webidl. Do not edit!

package js.html;

/**
	The `HTMLEmbedElement` interface, which provides special properties (beyond the regular `HTMLElement` interface it also has available to it by inheritance) for manipulating `embed` elements.

	Documentation [HTMLEmbedElement](https://developer.mozilla.org/en-US/docs/Web/API/HTMLEmbedElement) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/HTMLEmbedElement$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/HTMLEmbedElement>
**/
@:native("HTMLEmbedElement")
extern class EmbedElement extends Element
{
	
	/**
		Is a `DOMString` that reflects the `src` HTML attribute, containing the address of the resource.
	**/
	var src : String;
	
	/**
		Is a `DOMString` that reflects the `type` HTML attribute, containing the type of the resource.
	**/
	var type : String;
	
	/**
		Is a `DOMString` that reflects the `width` HTML attribute, containing the displayed width of the resource.
	**/
	var width : String;
	
	/**
		Is a `DOMString` representing an enumerated property indicating alignment of the element's contents with respect to the surrounding context. The possible values are `"left"`, `"right"`, `"center"`, and `"justify"`.
	**/
	var height : String;
	
	/**
		Is a `DOMString` reflecting the `height` HTML attribute, containing the displayed height of the resource.
	**/
	var align : String;
	
	/**
		Is a `DOMString` representing the name of the embedded object.
	**/
	var name : String;
	
	function getSVGDocument() : HTMLDocument;
}