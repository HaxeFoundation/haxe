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

// This file is generated from mozilla\HTMLIFrameElement.webidl. Do not edit!

package js.html;

/**
	The `HTMLIFrameElement` interface provides special properties and methods (beyond those of the `HTMLElement` interface it also has available to it by inheritance) for manipulating the layout and presentation of inline frame elements.

	Documentation [HTMLIFrameElement](https://developer.mozilla.org/en-US/docs/Web/API/HTMLIFrameElement) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/HTMLIFrameElement$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/HTMLIFrameElement>
**/
@:native("HTMLIFrameElement")
extern class IFrameElement extends Element
{
	
	/**
		Is a `DOMString` that reflects the `src` HTML attribute, containing the address of the content to be embedded.
	**/
	var src : String;
	
	/**
		Is a `DOMString` that represents the content to display in the frame.
	**/
	var srcdoc : String;
	
	/**
		Is a `DOMString` that reflects the `name` HTML attribute, containing a name by which to refer to the frame.
	**/
	var name : String;
	
	/**
		Is a `DOMSettableTokenList` that reflects the `sandbox` HTML attribute, indicating extra restrictions on the behavior of the nested content.
	**/
	var sandbox(default,null) : DOMTokenList;
	var allowFullscreen : Bool;
	
	/**
		Is a `DOMString` that reflects the `width` HTML attribute, indicating the width of the frame.
	**/
	var width : String;
	
	/**
		Is a `DOMString` that reflects the `height` HTML attribute, indicating the height of the frame.
	**/
	var height : String;
	
	/**
		Returns a `Document`, the active document in the inline frame's nested browsing context.
	**/
	var contentDocument(default,null) : HTMLDocument;
	
	/**
		Returns a `WindowProxy`, the window proxy for the nested browsing context.
	**/
	var contentWindow(default,null) : Window;
	
	/**
		Is a `DOMString` that specifies the alignment of the frame with respect to the surrounding context.
	**/
	var align : String;
	
	/**
		Is a `DOMString` that indicates whether the browser should provide scrollbars for the frame.
	**/
	var scrolling : String;
	
	/**
		Is a `DOMString` that indicates whether to create borders between frames.
	**/
	var frameBorder : String;
	
	/**
		Is a `DOMString` that contains the URI of a long description of the frame.
	**/
	var longDesc : String;
	
	/**
		Is a `DOMString` being the height of the frame margin.
	**/
	var marginHeight : String;
	
	/**
		Is a `DOMString` being the width of the frame margin.
	**/
	var marginWidth : String;
	
	function getSVGDocument() : HTMLDocument;
}