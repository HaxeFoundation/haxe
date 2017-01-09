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

// This file is generated from mozilla\CanvasRenderingContext2D.webidl. Do not edit!

package js.html;

/**
	The `Path2D` interface of the Canvas 2D API is used to declare paths that are then later used on `CanvasRenderingContext2D` objects. The path methods of the `CanvasRenderingContext2D` interface are present on this interface as well and are allowing you to create paths that you can retain and replay as required on a canvas.

	Documentation [Path2D](https://developer.mozilla.org/en-US/docs/Web/API/Path2D) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/Path2D$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/Path2D>
**/
@:native("Path2D")
extern class Path2D
{
	/** @throws DOMError */
	@:overload( function() : Void {} )
	@:overload( function( other : Path2D ) : Void {} )
	function new( pathString : String ) : Void;
	
	/**
		Adds a path to the current path.
	**/
	function addPath( path : Path2D, ?transformation : js.html.svg.Matrix ) : Void;
	function closePath() : Void;
	function moveTo( x : Float, y : Float ) : Void;
	function lineTo( x : Float, y : Float ) : Void;
	function quadraticCurveTo( cpx : Float, cpy : Float, x : Float, y : Float ) : Void;
	function bezierCurveTo( cp1x : Float, cp1y : Float, cp2x : Float, cp2y : Float, x : Float, y : Float ) : Void;
	/** @throws DOMError */
	function arcTo( x1 : Float, y1 : Float, x2 : Float, y2 : Float, radius : Float ) : Void;
	function rect( x : Float, y : Float, w : Float, h : Float ) : Void;
	/** @throws DOMError */
	function arc( x : Float, y : Float, radius : Float, startAngle : Float, endAngle : Float, ?anticlockwise : Bool = false ) : Void;
	/** @throws DOMError */
	function ellipse( x : Float, y : Float, radiusX : Float, radiusY : Float, rotation : Float, startAngle : Float, endAngle : Float, ?anticlockwise : Bool = false ) : Void;
}