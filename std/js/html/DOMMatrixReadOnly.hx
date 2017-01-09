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
	The `DOMMatrixReadOnly` interface represents 4x4 matrices, suitable for 2D and 3D operations. If this interface defines only read-only matrices, the `DOMMatrix` interface which inherits from it, add all the properties and the methods to allow to have modifiable matrices.

	Documentation [DOMMatrixReadOnly](https://developer.mozilla.org/en-US/docs/Web/API/DOMMatrixReadOnly) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/DOMMatrixReadOnly$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/DOMMatrixReadOnly>
**/
@:native("DOMMatrixReadOnly")
extern class DOMMatrixReadOnly
{
	
	/**
		Are <code>double</code> representing each component of a 4x4 matrix needed for 2D rotations and translations. They are aliases for some components of the 4x4 matrix:
		 <table class="standard-table">
		  
		   <tr>
		    2D
		    3D equivalent
		   </tr>
		  
		  
		   <tr>
		    <td><code>a</code></td>
		    <td><code>m11</code></td>
		   </tr>
		   <tr>
		    <td><code>b</code></td>
		    <td><code>m12</code></td>
		   </tr>
		   <tr>
		    <td><code>c</code></td>
		    <td><code>m21</code></td>
		   </tr>
		   <tr>
		    <td><code>d</code></td>
		    <td><code>m22</code></td>
		   </tr>
		   <tr>
		    <td><code>e</code></td>
		    <td><code>m41</code></td>
		   </tr>
		   <tr>
		    <td><code>f</code></td>
		    <td><code>m42</code></td>
		   </tr>
		  
		 </table>
		 They are read-only, but their counterpart, with the same name, in <code>DOMMatrix</code> aren't.
	**/
	var a(default,null) : Float;
	var b(default,null) : Float;
	var c(default,null) : Float;
	var d(default,null) : Float;
	var e(default,null) : Float;
	var f(default,null) : Float;
	
	/**
		Are `double` representing each component of a 4x4 matrix. They are read-only, but their counterpart, with the same name, in `DOMMatrix` aren't.
	**/
	var m11(default,null) : Float;
	var m12(default,null) : Float;
	var m13(default,null) : Float;
	var m14(default,null) : Float;
	var m21(default,null) : Float;
	var m22(default,null) : Float;
	var m23(default,null) : Float;
	var m24(default,null) : Float;
	var m31(default,null) : Float;
	var m32(default,null) : Float;
	var m33(default,null) : Float;
	var m34(default,null) : Float;
	var m41(default,null) : Float;
	var m42(default,null) : Float;
	var m43(default,null) : Float;
	var m44(default,null) : Float;
	
	/**
		Is a `Boolean` indicating if the matrix contains a 2D matrix and only accept 2D transformations.
	**/
	var is2D(default,null) : Bool;
	var identity(default,null) : Bool;
	
	
	/**
		Returns a `DOMMatrix` containing a new matrix being the result of the matrix being translated by the given vector. The original matrix is not modified.
	**/
	function translate( tx : Float, ty : Float, ?tz : Float = 0.0 ) : DOMMatrix;
	
	/**
		Returns a `DOMMatrix` containing a new matrix being the result of the matrix x and y dimensions being scaled by the given factor, centered on the origin given. The original matrix is not modified.
	**/
	function scale( scale : Float, ?originX : Float = 0.0, ?originY : Float = 0.0 ) : DOMMatrix;
	
	/**
		Returns a `DOMMatrix` containing a new matrix being the result of the matrix x, y and z dimension being scaled by the given factor, centered on the origin given. The original matrix is not modified.
	**/
	function scale3d( scale : Float, ?originX : Float = 0.0, ?originY : Float = 0.0, ?originZ : Float = 0.0 ) : DOMMatrix;
	
	/**
		Returns a `DOMMatrix` containing a new matrix being the result of the matrix x, y and z dimension being scaled by the given factor for each dimension, centered on the origin given. The original matrix is not modified.
	**/
	function scaleNonUniform( scaleX : Float, ?scaleY : Float = 1.0, ?scaleZ : Float = 1.0, ?originX : Float = 0.0, ?originY : Float = 0.0, ?originZ : Float = 0.0 ) : DOMMatrix;
	
	/**
		Returns a `DOMMatrix` containing a new matrix being the result of the original matrix being rotated by the given angle, with the rotation centered on the origin given. The original matrix is not modified.
	**/
	function rotate( angle : Float, ?originX : Float = 0.0, ?originY : Float = 0.0 ) : DOMMatrix;
	
	/**
		Returns a `DOMMatrix` containing a new matrix being the result of the original matrix being rotated by the angle between the given vector and (1,0), centered on the origin given. The original matrix is not modified.
	**/
	function rotateFromVector( x : Float, y : Float ) : DOMMatrix;
	
	/**
		Returns a `DOMMatrix` containing a new matrix being the result of the original matrix being rotated by the given angle and the give vector. The original matrix is not modified.
	**/
	function rotateAxisAngle( x : Float, y : Float, z : Float, angle : Float ) : DOMMatrix;
	
	/**
		Returns a `DOMMatrix` containing a new matrix being the result of the original matrix being skewed along the x-axis by the given factor. The original matrix is not modified.
	**/
	function skewX( sx : Float ) : DOMMatrix;
	
	/**
		Returns a `DOMMatrix` containing a new matrix being the result of the original matrix being skewed along the y-axis by the given factor. The original matrix is not modified.
	**/
	function skewY( sy : Float ) : DOMMatrix;
	
	/**
		Returns a `DOMMatrix` containing a new matrix being the result of the original matrix being multiplied by the given `DOMMatrix`. The original matrix is not modified.
	**/
	function multiply( other : DOMMatrix ) : DOMMatrix;
	
	/**
		Returns a `DOMMatrix` containing a new matrix being the result of the original matrix being flipped around the x-axis, that is multiplied by the `DOMMatrix(-1, 0, 0, 1, 0, 0)`. The original matrix is not modified.
	**/
	function flipX() : DOMMatrix;
	
	/**
		Returns a `DOMMatrix` containing a new matrix being the result of the original matrix being flipped around the y-axis, that is multiplied by the `DOMMatrix(1, 0, 0, -1, 0, 0)`. The original matrix is not modified.
	**/
	function flipY() : DOMMatrix;
	function inverse() : DOMMatrix;
	
	/**
		Returns a `DOMPoint` that is the point given in parameter multiplied by the matrix. Bot the original point and the matrix aren't modified.
	**/
	function transformPoint( ?point : DOMPointInit ) : DOMPoint;
	/** @throws DOMError */
	
	/**
		Returns a `Float32Array` containing the 6 components (`a`, `b`, `c`, `d`, `e`, `f`) in the case of a 2D matrix or the 16 components (`m11`, `m12`, `m13`, `m14`, `m21`, `m22`, `m23`, `m24`, `m31`, `m32`, `m33`, `m34`, `m41`, `m42`, `m43`, `m44`) for a 3D matrix.
	**/
	function toFloat32Array() : Float32Array;
	/** @throws DOMError */
	
	/**
		Returns a `Float64Array` containing the 6 components (`a`, `b`, `c`, `d`, `e`, `f`) in the case of a 2D matrix or the 16 components (`m11`, `m12`, `m13`, `m14`, `m21`, `m22`, `m23`, `m24`, `m31`, `m32`, `m33`, `m34`, `m41`, `m42`, `m43`, `m44`) for a 3D matrix.
	**/
	function toFloat64Array() : Float64Array;
}