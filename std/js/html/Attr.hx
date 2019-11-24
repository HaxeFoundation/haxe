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

// This file is generated from mozilla\Attr.webidl. Do not edit!

package js.html;

/**
	This type represents a DOM element's attribute as an object. In most DOM methods, you will probably directly retrieve the attribute as a string (e.g., `Element.getAttribute()`, but certain functions (e.g., `Element.getAttributeNode()`) or means of iterating give `Attr` types.

	Documentation [Attr](https://developer.mozilla.org/en-US/docs/Web/API/Attr) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/Attr$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/Attr>
**/
@:native("Attr")
extern class Attr extends Node {
	
	/**
		A `DOMString` representing the local part of the qualified name of the attribute.
	**/
	var localName(default,null) : String;
	
	/**
		The attribute's value.
	**/
	var value : String;
	
	/**
		The attribute's name.
	**/
	var name(default,null) : String;
	
	/**
		A `DOMString` representing the namespace URI of the attribute, or `null` if there is no namespace.
	**/
	var namespaceURI(default,null) : String;
	
	/**
		A `DOMString` representing the namespace prefix of the attribute, or `null` if no prefix is specified.
	**/
	var prefix(default,null) : String;
	
	/**
		This property always returns `true`. Originally, it returned `true `if the attribute was explicitly specified in the source code or by a script, and `false` if its value came from the default one defined in the document's DTD.
	**/
	var specified(default,null) : Bool;
	
	/**
		
			The element holding the attribute.
		
			
			Note: DOM Level 4 removed this property. The assumption was that since you get an `Attr` object from an `Element`, you should already know the associated element.
		
			As that doesn't hold true in cases like `Attr` objects being returned by `Document.evaluate`, the DOM Living Standard reintroduced the property.
		
			Gecko outputs a deprecation note starting from Gecko 7.0 `7.0`. This note was removed again in Gecko 49.0 `49.0`.
			
			
	**/
	var ownerElement(default,null) : Element;
	
}