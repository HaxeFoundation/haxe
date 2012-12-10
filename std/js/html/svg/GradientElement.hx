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

/** The <code>SVGGradient</code> interface is a base interface used by <code><a rel="custom" href="https://developer.mozilla.org/en/DOM/SVGLinearGradientElement">SVGLinearGradientElement</a></code>
 and <code><a rel="custom" href="https://developer.mozilla.org/en/DOM/SVGRadialGradientElement">SVGRadialGradientElement</a></code>
.<br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/SVGGradientElement">MDN</a>. */
@:native("SVGGradientElement")
extern class GradientElement extends Element
{
    /** Corresponds to value <em>pad</em>. */
    static inline var SVG_SPREADMETHOD_PAD :Int = 1;

    /** Corresponds to value <em>reflect</em>. */
    static inline var SVG_SPREADMETHOD_REFLECT :Int = 2;

    /** Corresponds to value <em>repeat</em>. */
    static inline var SVG_SPREADMETHOD_REPEAT :Int = 3;

    /** The type is not one of predefined types. It is invalid to attempt to define a new value of this type or to attempt to switch an existing value to this type. */
    static inline var SVG_SPREADMETHOD_UNKNOWN :Int = 0;

    /** Corresponds to attribute 
<code><a rel="internal" href="https://developer.mozilla.org/en/SVG/Attribute/gradientTransform" class="new">gradientTransform</a></code> on the given element. */
    var gradientTransform (default,null) :AnimatedTransformList;

    /** Corresponds to attribute 
<code><a rel="internal" href="https://developer.mozilla.org/en/SVG/Attribute/gradientUnits" class="new">gradientUnits</a></code> on the given element. Takes one of the constants defined in <code><a rel="internal" href="https://developer.mozilla.org/Article_not_found?uri=en/DOM/SVGUnitTypes" class="new">SVGUnitTypes</a></code>
. */
    var gradientUnits (default,null) :AnimatedEnumeration;

    /** Corresponds to attribute 
<code><a rel="internal" href="https://developer.mozilla.org/en/SVG/Attribute/spreadMethod" class="new">spreadMethod</a></code> on the given element. One of the Spread Method Types defined on this interface. */
    var spreadMethod (default,null) :AnimatedEnumeration;

}
