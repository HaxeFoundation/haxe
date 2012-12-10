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

/** <p><code>SVGTransform</code> is the interface for one of the component transformations within an <code><a rel="custom" href="https://developer.mozilla.org/en/DOM/SVGTransformList">SVGTransformList</a></code>
; thus, an <code>SVGTransform</code> object corresponds to a single component (e.g., <code>scale(…)</code> or <code>matrix(…)</code>) within a 
<code><a rel="custom" href="https://developer.mozilla.org/en/SVG/Attribute/transform">transform</a></code> attribute.</p>
<p>An <code>SVGTransform</code> object can be designated as read only, which means that attempts to modify the object will result in an exception being thrown.</p><br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/SVGTransform">MDN</a>. */
@:native("SVGTransform")
extern class Transform
{
    /** A <code>matrix(…)</code> transformation */
    static inline var SVG_TRANSFORM_MATRIX :Int = 1;

    static inline var SVG_TRANSFORM_ROTATE :Int = 4;

    /** A <code>scale(…)</code> transformation */
    static inline var SVG_TRANSFORM_SCALE :Int = 3;

    static inline var SVG_TRANSFORM_SKEWX :Int = 5;

    static inline var SVG_TRANSFORM_SKEWY :Int = 6;

    /** A <code>translate(…)</code> transformation */
    static inline var SVG_TRANSFORM_TRANSLATE :Int = 2;

    /** The unit type is not one of predefined unit types. It is invalid to attempt to define a new value of this type or to attempt to switch an existing value to this type. */
    static inline var SVG_TRANSFORM_UNKNOWN :Int = 0;

    /** A convenience attribute for <code>SVG_TRANSFORM_ROTATE</code>, <code>SVG_TRANSFORM_SKEWX</code> and <code>SVG_TRANSFORM_SKEWY</code>. It holds the angle that was specified.<br> <br> For <code>SVG_TRANSFORM_MATRIX</code>, <code>SVG_TRANSFORM_TRANSLATE</code> and <code>SVG_TRANSFORM_SCALE</code>, <code>angle</code> will be zero. */
    var angle (default,null) :Float;

    /** <p>The matrix that represents this transformation. The matrix object is live, meaning that any changes made to the <code>SVGTransform</code> object are immediately reflected in the matrix object and vice versa. In case the matrix object is changed directly (i.e., without using the methods on the <code>SVGTransform</code> interface itself) then the type of the <code>SVGTransform</code> changes to <code>SVG_TRANSFORM_MATRIX</code>.</p> <ul> <li>For <code>SVG_TRANSFORM_MATRIX</code>, the matrix contains the a, b, c, d, e, f values supplied by the user.</li> <li>For <code>SVG_TRANSFORM_TRANSLATE</code>, e and f represent the translation amounts (a=1, b=0, c=0 and d=1).</li> <li>For <code>SVG_TRANSFORM_SCALE</code>, a and d represent the scale amounts (b=0, c=0, e=0 and f=0).</li> <li>For <code>SVG_TRANSFORM_SKEWX</code> and <code>SVG_TRANSFORM_SKEWY</code>, a, b, c and d represent the matrix which will result in the given skew (e=0 and f=0).</li> <li>For <code>SVG_TRANSFORM_ROTATE</code>, a, b, c, d, e and f together represent the matrix which will result in the given rotation. When the rotation is around the center point (0, 0), e and f will be zero.</li> </ul> */
    var matrix (default,null) :Matrix;

    /** The type of the value as specified by one of the SVG_TRANSFORM_* constants defined on this interface. */
    var type (default,null) :Int;

    function setMatrix (matrix :Matrix) :Void;

    function setRotate (angle :Float, cx :Float, cy :Float) :Void;

    function setScale (sx :Float, sy :Float) :Void;

    function setSkewX (angle :Float) :Void;

    function setSkewY (angle :Float) :Void;

    function setTranslate (tx :Float, ty :Float) :Void;

}
