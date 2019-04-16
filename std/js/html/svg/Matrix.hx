/*
 * Copyright (C)2005-2019 Haxe Foundation
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

// This file is generated from mozilla\SVGMatrix.webidl. Do not edit!

package js.html.svg;

/**
	Many of SVG's graphics operations utilize 2x3 matrices of the form:

	Documentation [SVGMatrix](https://developer.mozilla.org/en-US/docs/Web/API/SVGMatrix) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/SVGMatrix$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/SVGMatrix>
**/
@:native("SVGMatrix")
extern class Matrix {
	
	/**
		A float representing the a component of the matrix.
	**/
	var a : Float;
	
	/**
		A float representing the b component of the matrix.
	**/
	var b : Float;
	
	/**
		A float representing the c component of the matrix.
	**/
	var c : Float;
	
	/**
		A float representing the d component of the matrix.
	**/
	var d : Float;
	
	/**
		A float representing the e component of the matrix.
	**/
	var e : Float;
	
	/**
		A float representing the f component of the matrix.
	**/
	var f : Float;
	
	
	/**
		Performs matrix multiplication. This matrix is post-multiplied by another matrix, returning the resulting new matrix as `SVGMatrix`.
	**/
	function multiply( secondMatrix : Matrix ) : Matrix;
	
	/**
		Returns the inverse matrix as `SVGMatrix`.
		@throws DOMError
	**/
	function inverse() : Matrix;
	
	/**
		Post-multiplies a translation transformation on the current matrix and returns the resulting matrix as `SVGMatrix`.
	**/
	function translate( x : Float, y : Float ) : Matrix;
	
	/**
		Post-multiplies a uniform scale transformation on the current matrix and returns the resulting matrix as `SVGMatrix`.
	**/
	function scale( scaleFactor : Float ) : Matrix;
	
	/**
		Post-multiplies a non-uniform scale transformation on the current matrix and returns the resulting matrix as `SVGMatrix`.
	**/
	function scaleNonUniform( scaleFactorX : Float, scaleFactorY : Float ) : Matrix;
	
	/**
		Post-multiplies a rotation transformation on the current matrix and returns the resulting matrix as `SVGMatrix`.
	**/
	function rotate( angle : Float ) : Matrix;
	
	/**
		Post-multiplies a rotation transformation on the current matrix and returns the resulting matrix as `SVGMatrix`. The rotation angle is determined by taking (+/-) atan(y/x). The direction of the vector (x, y) determines whether the positive or negative angle value is used.
		@throws DOMError
	**/
	function rotateFromVector( x : Float, y : Float ) : Matrix;
	
	/**
		Post-multiplies the transformation [-1 0 0 1 0 0] and returns the resulting matrix as `SVGMatrix`.
	**/
	function flipX() : Matrix;
	
	/**
		Post-multiplies the transformation [1 0 0 -1 0 0] and returns the resulting matrix as `SVGMatrix`.
	**/
	function flipY() : Matrix;
	
	/**
		Post-multiplies a skewX transformation on the current matrix and returns the resulting matrix as `SVGMatrix`.
		@throws DOMError
	**/
	function skewX( angle : Float ) : Matrix;
	
	/**
		Post-multiplies a skewY transformation on the current matrix and returns the resulting matrix as `SVGMatrix`.
		@throws DOMError
	**/
	function skewY( angle : Float ) : Matrix;
}