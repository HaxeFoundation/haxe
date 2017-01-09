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

// This file is generated from mozilla\HTMLLinkElement.webidl. Do not edit!

package js.html;

/**
	The `HTMLLinkElement` interface represents reference information for external resources and the relationship of those resources to a document and vice-versa. This object inherits all of the properties and methods of the `HTMLElement` interface.

	Documentation [HTMLLinkElement](https://developer.mozilla.org/en-US/docs/Web/API/HTMLLinkElement) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/HTMLLinkElement$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/HTMLLinkElement>
**/
@:native("HTMLLinkElement")
extern class LinkElement extends Element
{
	
	/**
		Is a `Boolean` which represents whether the link is disabled; currently only used with style sheet links.
	**/
	var disabled : Bool;
	
	/**
		Is a `DOMString` representing the URI for the target resource.
	**/
	var href : String;
	
	/**
		Is a `DOMString` that corresponds to the CORS setting for this link element. See CORS settings attributes for details.
	**/
	var crossOrigin : String;
	
	/**
		Is a `DOMString` representing the forward relationship of the linked resource from the document to the resource.
	**/
	var rel : String;
	
	/**
		Is a `DOMTokenList` that reflects the `rel` HTML attribute, as a list of tokens.
	**/
	var relList(default,null) : DOMTokenList;
	
	/**
		Is a `DOMString` representing a list of one or more media formats to which the resource applies.
	**/
	var media : String;
	
	/**
		Is a `DOMString` representing the language code for the linked resource.
	**/
	var hreflang : String;
	
	/**
		Is a `DOMString` representing the MIME type of the linked resource.
	**/
	var type : String;
	
	/**
		Is a `DOMSettableTokenList` that reflects the `sizes` HTML attribute, as a list of tokens.
	**/
	var sizes(default,null) : DOMTokenList;
	
	/**
		Is a `DOMString` representing the character encoding for the target resource.
	**/
	var charset : String;
	
	/**
		Is a `DOMString` representing the reverse relationship of the linked resource from the resource to the document.
	**/
	var rev : String;
	
	/**
		Is a `DOMString` representing the name of the target frame to which the resource applies.
	**/
	var target : String;
	@:native("import")
	var import_(default,null) : HTMLDocument;
	var integrity : String;
	var sheet(default,null) : StyleSheet;
	
}