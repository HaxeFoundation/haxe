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

/** <p>Many of SVG's graphics operations utilize 2x3 matrices of the form:</p>
<pre>[a c e]
[b d f]</pre>
<p>which, when expanded into a 3x3 matrix for the purposes of matrix arithmetic, become:</p>
<pre>[a c e]
[b d f]
[0 0 1]
</pre>
<p>An <code>SVGMatrix</code> object can be designated as read only, which means that attempts to modify the object will result in an exception being thrown.</p><br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/SVGMatrix">MDN</a>. */
@:native("SVGMatrix")
extern class Matrix
{
	var a : Float;

	var b : Float;

	var c : Float;

	var d : Float;

	var e : Float;

	var f : Float;

	function flipX() : Matrix;

	function flipY() : Matrix;

	function inverse() : Matrix;

	function multiply( secondMatrix : Matrix ) : Matrix;

	function rotate( angle : Float ) : Matrix;

	function rotateFromVector( x : Float, y : Float ) : Matrix;

	function scale( scaleFactor : Float ) : Matrix;

	function scaleNonUniform( scaleFactorX : Float, scaleFactorY : Float ) : Matrix;

	function skewX( angle : Float ) : Matrix;

	function skewY( angle : Float ) : Matrix;

	function translate( x : Float, y : Float ) : Matrix;

}
