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

/** The <code>base</code> object exposes the <a class=" external" title="http://www.w3.org/TR/html5/semantics.html#htmlbaseelement" rel="external" href="http://www.w3.org/TR/html5/semantics.html#htmlbaseelement" target="_blank">HTMLBaseElement</a> (or 
<span><a rel="custom" href="https://developer.mozilla.org/en/HTML">HTML 4</a></span> <a class="external" target="_blank" rel="external" href="http://www.w3.org/TR/DOM-Level-2-HTML/html.html#ID-73629039" title="http://www.w3.org/TR/DOM-Level-2-HTML/html.html#ID-73629039">HTMLBaseElement</a>) interface which contains the base URI&nbsp;for a document.&nbsp; This object inherits all of the properties and methods as described in the <a class="internal" title="en/DOM/element" rel="internal" href="https://developer.mozilla.org/en/DOM/element">element</a> section.<br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/HTMLBaseElement">MDN</a>. */
@:native("HTMLBaseElement")
extern class BaseElement extends Element
{
	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/base#attr-href">href</a></code>
 HTML attribute, containing a base URL for relative URLs in the document. */
	var href : String;

	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/base#attr-target">target</a></code>
 HTML attribute, containing a default target browsing context or frame for elements that do not have a target reference specified. */
	var target : String;

}
