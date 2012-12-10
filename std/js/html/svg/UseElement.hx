/*
 * Copyright (C)2005-2012 Haxe Foundation
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

/** The <code>SVGUseElement</code> interface provides access to the properties of <code><a rel="custom" href="https://developer.mozilla.org/en/SVG/Element/use">&lt;use&gt;</a></code>
 elements, as well as methods to manipulate them.<br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/SVGUseElement">MDN</a>. */
@:native("SVGUseElement")
extern class UseElement extends Element
{
    /** If the 
<code><a rel="custom" href="https://developer.mozilla.org/en/SVG/Attribute/xlink%3Ahref">xlink:href</a></code> attribute is being animated, contains the current animated root of the instance tree. If the 
<code><a rel="custom" href="https://developer.mozilla.org/en/SVG/Attribute/xlink%3Ahref">xlink:href</a></code> attribute is not currently being animated, contains the same value as <code>instanceRoot</code>. See description of <code><a rel="internal" href="https://developer.mozilla.org/Article_not_found?uri=en/DOM/SVGElementInstance" class="new">SVGElementInstance</a></code>
 to learn more about the instance tree. */
    var animatedInstanceRoot (default,null) :ElementInstance;

    /** Corresponds to attribute 
<code><a rel="custom" href="https://developer.mozilla.org/en/SVG/Attribute/height">height</a></code> on the given <code><a rel="custom" href="https://developer.mozilla.org/en/SVG/Element/use">&lt;use&gt;</a></code>
 element. */
    var height (default,null) :AnimatedLength;

    /** The root of the instance tree. See description of <code><a rel="internal" href="https://developer.mozilla.org/Article_not_found?uri=en/DOM/SVGElementInstance" class="new">SVGElementInstance</a></code>
 to learn more about the instance tree. */
    var instanceRoot (default,null) :ElementInstance;

    /** Corresponds to attribute 
<code><a rel="custom" href="https://developer.mozilla.org/en/SVG/Attribute/width">width</a></code> on the given <code><a rel="custom" href="https://developer.mozilla.org/en/SVG/Element/use">&lt;use&gt;</a></code>
 element. */
    var width (default,null) :AnimatedLength;

    var x (default,null) :AnimatedLength;

    var y (default,null) :AnimatedLength;

}
