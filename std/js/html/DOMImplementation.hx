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

// This file is generated from mozilla\DOMImplementation.webidl. Do not edit!

package js.html;

/**
	The `DOMImplementation` interface represent an object providing methods which are not dependent on any particular document. Such an object is returned by the `Document.implementation` property.

	Documentation [DOMImplementation](https://developer.mozilla.org/en-US/docs/Web/API/DOMImplementation) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/DOMImplementation$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/DOMImplementation>
**/
@:native("DOMImplementation")
extern class DOMImplementation
{
	
	/**
		Returns a `Boolean` indicating if a given feature is supported or not. This function is unreliable and kept for compatibility purpose alone: except for SVG-related queries, it always returns `true`. Old browsers are very inconsistent in their behavior.
	**/
	function hasFeature( feature : String, version : String ) : Bool;
	/** @throws DOMError */
	
	/**
		Creates and returns a `DocumentType`.
	**/
	function createDocumentType( qualifiedName : String, publicId : String, systemId : String ) : DocumentType;
	/** @throws DOMError */
	
	/**
		Creates and returns an `XMLDocument`.
	**/
	function createDocument( namespace_ : String, qualifiedName : String, ?doctype : DocumentType ) : HTMLDocument;
	/** @throws DOMError */
	
	/**
		Creates and returns an HTML `Document`.
	**/
	function createHTMLDocument( ?title : String ) : HTMLDocument;
}