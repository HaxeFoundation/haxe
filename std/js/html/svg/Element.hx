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
package js.html.svg;

/** All of the SVG DOM interfaces that correspond directly to elements in the SVG language derive from the <code>SVGElement</code> interface.<br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/SVGElement">MDN</a>. */
@:native("SVGElement")
extern class Element extends js.html.Element
{
	/** The nearest ancestor <code><a rel="custom" href="https://developer.mozilla.org/en/SVG/Element/svg">&lt;svg&gt;</a></code>
 element. <code>Null</code> if the given element is the outermost svg element. */
	var ownerSVGElement(default,null) : SVGElement;

	/** The element which established the current viewport. Often, the nearest ancestor <code><a rel="custom" href="https://developer.mozilla.org/en/SVG/Element/svg">&lt;svg&gt;</a></code>
 element. <code>Null</code> if the given element is the outermost svg element. */
	var viewportElement(default,null) : Element;

	/** Corresponds to attribute 
<code><a rel="internal" href="https://developer.mozilla.org/en/SVG/Attribute/xml%3Abase" class="new">xml:base</a></code> on the given element. Setter throws DOMException. */
	var xmlbase : String;

}
