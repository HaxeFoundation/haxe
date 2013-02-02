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

/** In addition to text drawn in a straight line, SVG also includes the ability to place text along the shape of a <code><a rel="custom" href="https://developer.mozilla.org/en/SVG/Element/path">&lt;path&gt;</a></code>
 element. To specify that a block of text is to be rendered along the shape of a <code><a rel="custom" href="https://developer.mozilla.org/en/SVG/Element/path">&lt;path&gt;</a></code>
, include the given text within a <code>textPath</code> element which includes an <code>xlink:href</code> attribute with a reference to a <code><a rel="custom" href="https://developer.mozilla.org/en/SVG/Element/path">&lt;path&gt;</a></code>
 element.<br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/SVG/Element/textPath">MDN</a>. */
@:native("SVGTextPathElement")
extern class TextPathElement extends TextContentElement
{
	static inline var TEXTPATH_METHODTYPE_ALIGN : Int = 1;

	static inline var TEXTPATH_METHODTYPE_STRETCH : Int = 2;

	static inline var TEXTPATH_METHODTYPE_UNKNOWN : Int = 0;

	static inline var TEXTPATH_SPACINGTYPE_AUTO : Int = 1;

	static inline var TEXTPATH_SPACINGTYPE_EXACT : Int = 2;

	static inline var TEXTPATH_SPACINGTYPE_UNKNOWN : Int = 0;

	var method (default,null) : AnimatedEnumeration;

	var spacing (default,null) : AnimatedEnumeration;

	var startOffset (default,null) : AnimatedLength;

}
