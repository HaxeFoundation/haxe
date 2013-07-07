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
package js.html;

@:native("CSSMatrix")
extern class CSSMatrix
{
	var a : Float;

	var b : Float;

	var c : Float;

	var d : Float;

	var e : Float;

	var f : Float;

	var m11 : Float;

	var m12 : Float;

	var m13 : Float;

	var m14 : Float;

	var m21 : Float;

	var m22 : Float;

	var m23 : Float;

	var m24 : Float;

	var m31 : Float;

	var m32 : Float;

	var m33 : Float;

	var m34 : Float;

	var m41 : Float;

	var m42 : Float;

	var m43 : Float;

	var m44 : Float;

	function new() : Void;

	function inverse() : CSSMatrix;

	function multiply( secondMatrix : CSSMatrix ) : CSSMatrix;

	function rotate( rotX : Float, rotY : Float, rotZ : Float ) : CSSMatrix;

	function rotateAxisAngle( x : Float, y : Float, z : Float, angle : Float ) : CSSMatrix;

	function scale( scaleX : Float, scaleY : Float, scaleZ : Float ) : CSSMatrix;

	function setMatrixValue( string : String ) : Void;

	function skewX( angle : Float ) : CSSMatrix;

	function skewY( angle : Float ) : CSSMatrix;

	function toString() : String;

	function translate( x : Float, y : Float, z : Float ) : CSSMatrix;

}
