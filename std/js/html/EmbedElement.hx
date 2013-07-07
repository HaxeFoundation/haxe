/*
 * Copyright (C)2005-2013 Haxe Foundation
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

// This file is generated, do not edit!
package js.html;

/** <strong>Note:</strong>&nbsp;This topic describes the HTMLEmbedElement interface as defined in the HTML5 standard. It does not address earlier, non-standardized version of the interface.<br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/HTMLEmbedElement">MDN</a>. */
@:native("HTMLEmbedElement")
extern class EmbedElement extends Element
{
	var align : String;

	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/embed#attr-height">height</a></code>
 HTML&nbsp;attribute, containing the displayed height of the resource. */
	var height : String;

	var name : String;

	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/embed#attr-src">src</a></code>
 HTML&nbsp;attribute, containing the address of the resource. */
	var src : String;

	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/embed#attr-type">type</a></code>
 HTML&nbsp;attribute, containing the type of the resource. */
	var type : String;

	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/embed#attr-width">width</a></code>
 HTML&nbsp;attribute, containing the displayed width of the resource. */
	var width : String;

	function getSVGDocument() : js.html.svg.Document;

}
