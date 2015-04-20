/*
 * Copyright (C)2005-2015 Haxe Foundation
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

// This file is generated from mozilla/XSLTProcessor.webidl line 9:0. Do not edit!

package js.html;

@:native("XSLTProcessor")
extern class XSLTProcessor
{
	/** @throws DOMError */
	function new() : Void;
	/** @throws DOMError */
	function importStylesheet( style : Node ) : Void;
	/** @throws DOMError */
	function transformToFragment( source : Node, output : HTMLDocument ) : DocumentFragment;
	/** @throws DOMError */
	function transformToDocument( source : Node ) : HTMLDocument;
	/** @throws DOMError */
	function setParameter( namespaceURI : String, localName : String, value : Dynamic ) : Void;
	/** @throws DOMError */
	function getParameter( namespaceURI : String, localName : String ) : Dynamic/*MISSING nsIVariant*/;
	/** @throws DOMError */
	function removeParameter( namespaceURI : String, localName : String ) : Void;
	function clearParameters() : Void;
	function reset() : Void;
}