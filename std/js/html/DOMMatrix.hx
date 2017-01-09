/*
 * Copyright (C)2005-2017 Haxe Foundation
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

// This file is generated from mozilla\DOMMatrix.webidl. Do not edit!

package js.html;

/**
	The `DOMMatrix` interface represents 4x4 matrices, suitable for 2D and 3D operations.

	Documentation [DOMMatrix](https://developer.mozilla.org/en-US/docs/Web/API/DOMMatrix) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/DOMMatrix$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/DOMMatrix>
**/
@:native("DOMMatrix")
extern class DOMMatrix extends DOMMatrixReadOnly
{
	/** @throws DOMError */
	@:overload( function() : Void {} )
	@:overload( function( transformList : String ) : Void {} )
	@:overload( function( other : DOMMatrixReadOnly ) : Void {} )
	@:overload( function( array32 : Float32Array ) : Void {} )
	@:overload( function( array64 : Float64Array ) : Void {} )
	function new( numberSequence : Array<Float> ) : Void;
	function multiplySelf( other : DOMMatrix ) : DOMMatrix;
	function preMultiplySelf( other : DOMMatrix ) : DOMMatrix;
	
	/**
		Returns itself, a `DOMMatrix`, with its new content being the result of the matrix being translated by the given vector.
	**/
	function translateSelf( tx : Float, ty : Float, ?tz : Float = 0.0 ) : DOMMatrix;
	
	/**
		Returns itself, a `DOMMatrix`, with its new content being the result of the matrix x and y dimensions being scaled by the given factor, centered on the origin given.
	**/
	function scaleSelf( scale : Float, ?originX : Float = 0.0, ?originY : Float = 0.0 ) : DOMMatrix;
	
	/**
		Returns itself, a `DOMMatrix`, with its new content being the result of the matrix x, y and z dimension being scaled by the given factor, centered on the origin given.
	**/
	function scale3dSelf( scale : Float, ?originX : Float = 0.0, ?originY : Float = 0.0, ?originZ : Float = 0.0 ) : DOMMatrix;
	
	/**
		Returns itself, a `DOMMatrix`, with its new content being the result of the matrix x, y and z dimension being scaled by the given factor for each dimension, centered on the origin given.
	**/
	function scaleNonUniformSelf( scaleX : Float, ?scaleY : Float = 1.0, ?scaleZ : Float = 1.0, ?originX : Float = 0.0, ?originY : Float = 0.0, ?originZ : Float = 0.0 ) : DOMMatrix;
	
	/**
		Returns itself, a `DOMMatrix`, with its new content being the result of the original matrix being rotated by the given angle, with the rotation centered on the origin given.
	**/
	function rotateSelf( angle : Float, ?originX : Float = 0.0, ?originY : Float = 0.0 ) : DOMMatrix;
	
	/**
		Returns itself, a `DOMMatrix`, with its new content being the result of the original matrix being rotated by the angle between the given vector and (1,0), centered on the origin given.
	**/
	function rotateFromVectorSelf( x : Float, y : Float ) : DOMMatrix;
	
	/**
		Returns itself, a `DOMMatrix`, with its new content being the result of the original matrix being rotated by the given angle and the give vector.
	**/
	function rotateAxisAngleSelf( x : Float, y : Float, z : Float, angle : Float ) : DOMMatrix;
	
	/**
		Returns itself, a `DOMMatrix`, with its new content being the result of the original matrix being skewed along the x-axis by the given factor.
	**/
	function skewXSelf( sx : Float ) : DOMMatrix;
	
	/**
		Returns itself, a `DOMMatrix`, with its new content being the result of the original matrix being skewed along the y-axis by the given factor.
	**/
	function skewYSelf( sy : Float ) : DOMMatrix;
	
	/**
		Returns itself,  a `DOMMatrix`, with its new content being the result of the original matrix being inverted. If the matrix cannot be inverted, all its components are set to `NaN` and `is2D()` returns `false`.
	**/
	function invertSelf() : DOMMatrix;
	/** @throws DOMError */
	
	/**
		Returns itself, a `DOMMatrix`, with its describing the matrix representing the same transformation as the CSS `transform` functions given in parameter.
	**/
	function setMatrixValue( transformList : String ) : DOMMatrix;
}