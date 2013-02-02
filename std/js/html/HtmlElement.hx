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

/** <p>The <code>html</code> object exposes the <a class=" external" title="http://www.w3.org/TR/html5/semantics.html#htmlhtmlelement" rel="external" href="http://www.w3.org/TR/html5/semantics.html#htmlhtmlelement" target="_blank">HTMLHtmlElement</a> (
<span><a rel="custom" href="https://developer.mozilla.org/en/HTML">HTML 4</a></span> <a target="_blank" class="external" rel="external" href="http://www.w3.org/TR/DOM-Level-2-HTML/html.html#ID-33759296" title="http://www.w3.org/TR/DOM-Level-2-HTML/html.html#ID-33759296">HTMLHtmlElement</a>) interface and serves as the root node for a given HTML&nbsp;document.&nbsp; This object inherits the properties and methods described in the <a title="en/DOM/element" class="internal" rel="internal" href="https://developer.mozilla.org/en/DOM/element">element</a> section.&nbsp; In 
<span><a rel="custom" href="https://developer.mozilla.org/en/HTML/HTML5">HTML5</a></span>, this interface inherits from HTMLElement, but provides no other members.</p>
<p>You can retrieve the <code>html</code> object for a document by obtaining the value of the <a class="internal" title="en/DOM/document.documentElement" rel="internal" href="https://developer.mozilla.org/en/DOM/document.documentElement"><code>document.documentElement</code></a> property.</p><br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/En/DOM/Html">MDN</a>. */
@:native("HTMLHtmlElement")
extern class HtmlElement extends Element
{
	var manifest : String;

	/** Version of the HTML&nbsp;Document Type Definition that governs this document. */
	var version : String;

}
