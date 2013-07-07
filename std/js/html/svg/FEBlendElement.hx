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

/** The <code>feBlend</code> filter composes two objects together ruled by a certain blending mode. This is similar to what is known from image editing software when blending two layers. The mode is defined by the 
<code><a rel="internal" href="https://developer.mozilla.org/en/SVG/Attribute/mode" class="new">mode</a></code> attribute.<br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/SVG/Element/feBlend">MDN</a>. */
@:native("SVGFEBlendElement")
extern class FEBlendElement extends Element
{
	static inline var SVG_FEBLEND_MODE_DARKEN : Int = 4;

	static inline var SVG_FEBLEND_MODE_LIGHTEN : Int = 5;

	static inline var SVG_FEBLEND_MODE_MULTIPLY : Int = 2;

	static inline var SVG_FEBLEND_MODE_NORMAL : Int = 1;

	static inline var SVG_FEBLEND_MODE_SCREEN : Int = 3;

	static inline var SVG_FEBLEND_MODE_UNKNOWN : Int = 0;

	var in1(default,null) : AnimatedString;

	var in2(default,null) : AnimatedString;

	var mode(default,null) : AnimatedEnumeration;

}
