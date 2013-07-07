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

/** The <code>SVGAnimatedTransformList</code> interface is used for attributes which take a list of numbers and which can be animated.<br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/SVGAnimatedTransformList">MDN</a>. */
@:native("SVGAnimatedTransformList")
extern class AnimatedTransformList
{
	/** A read only <code><a rel="custom" href="https://developer.mozilla.org/en/DOM/SVGTransformList">SVGTransformList</a></code>
 representing the current animated value of the given attribute. If the given attribute is not currently being animated, then the <code><a rel="custom" href="https://developer.mozilla.org/en/DOM/SVGTransformList">SVGTransformList</a></code>
 will have the same contents as <code>baseVal</code>. The object referenced by <code>animVal</code> will always be distinct from the one referenced by <code>baseVal</code>, even when the attribute is not animated. */
	var animVal(default,null) : TransformList;

	/** The base value of the given attribute before applying any animations. */
	var baseVal(default,null) : TransformList;

}
