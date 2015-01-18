/*
 * Copyright (C)2005-2015 Haxe Foundation
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

// This file is generated from mozilla/DOMMatrix.webidl line 89:0. Do not edit!

package js.html;

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
	function translateSelf( tx : Float, ty : Float, ?tz : Float = 0.0 ) : DOMMatrix;
	function scaleSelf( scale : Float, ?originX : Float = 0.0, ?originY : Float = 0.0 ) : DOMMatrix;
	function scale3dSelf( scale : Float, ?originX : Float = 0.0, ?originY : Float = 0.0, ?originZ : Float = 0.0 ) : DOMMatrix;
	function scaleNonUniformSelf( scaleX : Float, ?scaleY : Float = 1.0, ?scaleZ : Float = 1.0, ?originX : Float = 0.0, ?originY : Float = 0.0, ?originZ : Float = 0.0 ) : DOMMatrix;
	function rotateSelf( angle : Float, ?originX : Float = 0.0, ?originY : Float = 0.0 ) : DOMMatrix;
	function rotateFromVectorSelf( x : Float, y : Float ) : DOMMatrix;
	function rotateAxisAngleSelf( x : Float, y : Float, z : Float, angle : Float ) : DOMMatrix;
	function skewXSelf( sx : Float ) : DOMMatrix;
	function skewYSelf( sy : Float ) : DOMMatrix;
	function invertSelf() : DOMMatrix;
	/** @throws DOMError */
	function setMatrixValue( transformList : String ) : DOMMatrix;
}