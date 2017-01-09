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
	To get an object of this interface, call `getContext()` on a `canvas element`, supplying "2d" as the argument:

	Documentation [CanvasRenderingContext2D](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D>
**/
@:native("CanvasRenderingContext2D")
extern class CanvasRenderingContext2D
{
	var canvas(default,null) : CanvasElement;
	var globalAlpha : Float;
	var globalCompositeOperation : String;
	var strokeStyle : haxe.extern.EitherType<String,haxe.extern.EitherType<CanvasGradient,CanvasPattern>>;
	var fillStyle : haxe.extern.EitherType<String,haxe.extern.EitherType<CanvasGradient,CanvasPattern>>;
	var shadowOffsetX : Float;
	var shadowOffsetY : Float;
	var shadowBlur : Float;
	var shadowColor : String;
	var filter : String;
	var imageSmoothingEnabled : Bool;
	var lineWidth : Float;
	var lineCap : String;
	var lineJoin : String;
	var miterLimit : Float;
	var lineDashOffset : Float;
	var font : String;
	var textAlign : String;
	var textBaseline : String;
	
	function save() : Void;
	function restore() : Void;
	/** @throws DOMError */
	function scale( x : Float, y : Float ) : Void;
	/** @throws DOMError */
	function rotate( angle : Float ) : Void;
	/** @throws DOMError */
	function translate( x : Float, y : Float ) : Void;
	/** @throws DOMError */
	function transform( a : Float, b : Float, c : Float, d : Float, e : Float, f : Float ) : Void;
	/** @throws DOMError */
	function setTransform( a : Float, b : Float, c : Float, d : Float, e : Float, f : Float ) : Void;
	/** @throws DOMError */
	function resetTransform() : Void;
	function createLinearGradient( x0 : Float, y0 : Float, x1 : Float, y1 : Float ) : CanvasGradient;
	/** @throws DOMError */
	function createRadialGradient( x0 : Float, y0 : Float, r0 : Float, x1 : Float, y1 : Float, r1 : Float ) : CanvasGradient;
	/** @throws DOMError */
	function createPattern( image : haxe.extern.EitherType<ImageElement,haxe.extern.EitherType<CanvasElement,haxe.extern.EitherType<VideoElement,ImageBitmap>>>, repetition : String ) : CanvasPattern;
	function clearRect( x : Float, y : Float, w : Float, h : Float ) : Void;
	function fillRect( x : Float, y : Float, w : Float, h : Float ) : Void;
	function strokeRect( x : Float, y : Float, w : Float, h : Float ) : Void;
	function beginPath() : Void;
	@:overload( function( ?winding : CanvasWindingRule = "nonzero" ) : Void {} )
	function fill( path : Path2D, ?winding : CanvasWindingRule = "nonzero" ) : Void;
	@:overload( function() : Void {} )
	function stroke( path : Path2D ) : Void;
	/** @throws DOMError */
	function drawFocusIfNeeded( element : Element ) : Void;
	function drawCustomFocusRing( element : Element ) : Bool;
	@:overload( function( ?winding : CanvasWindingRule = "nonzero" ) : Void {} )
	function clip( path : Path2D, ?winding : CanvasWindingRule = "nonzero" ) : Void;
	@:overload( function( x : Float, y : Float, ?winding : CanvasWindingRule = "nonzero" ) : Bool {} )
	function isPointInPath( path : Path2D, x : Float, y : Float, ?winding : CanvasWindingRule = "nonzero" ) : Bool;
	@:overload( function( x : Float, y : Float ) : Bool {} )
	function isPointInStroke( path : Path2D, x : Float, y : Float ) : Bool;
	/** @throws DOMError */
	function fillText( text : String, x : Float, y : Float, ?maxWidth : Float ) : Void;
	/** @throws DOMError */
	function strokeText( text : String, x : Float, y : Float, ?maxWidth : Float ) : Void;
	/** @throws DOMError */
	function measureText( text : String ) : TextMetrics;
	/** @throws DOMError */
	@:overload( function( image : haxe.extern.EitherType<ImageElement,haxe.extern.EitherType<CanvasElement,haxe.extern.EitherType<VideoElement,ImageBitmap>>>, dx : Float, dy : Float ) : Void {} )
	@:overload( function( image : haxe.extern.EitherType<ImageElement,haxe.extern.EitherType<CanvasElement,haxe.extern.EitherType<VideoElement,ImageBitmap>>>, dx : Float, dy : Float, dw : Float, dh : Float ) : Void {} )
	function drawImage( image : haxe.extern.EitherType<ImageElement,haxe.extern.EitherType<CanvasElement,haxe.extern.EitherType<VideoElement,ImageBitmap>>>, sx : Float, sy : Float, sw : Float, sh : Float, dx : Float, dy : Float, dw : Float, dh : Float ) : Void;
	/** @throws DOMError */
	function addHitRegion( ?options : HitRegionOptions ) : Void;
	function removeHitRegion( id : String ) : Void;
	function clearHitRegions() : Void;
	/** @throws DOMError */
	@:overload( function( sw : Float, sh : Float ) : ImageData {} )
	function createImageData( imagedata : ImageData ) : ImageData;
	/** @throws DOMError */
	function getImageData( sx : Float, sy : Float, sw : Float, sh : Float ) : ImageData;
	/** @throws DOMError */
	@:overload( function( imagedata : ImageData, dx : Float, dy : Float ) : Void {} )
	function putImageData( imagedata : ImageData, dx : Float, dy : Float, dirtyX : Float, dirtyY : Float, dirtyWidth : Float, dirtyHeight : Float ) : Void;
	/** @throws DOMError */
	function setLineDash( segments : Array<Float> ) : Void;
	function getLineDash() : Array<Float>;
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