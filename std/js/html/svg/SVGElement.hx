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

// This file is generated from mozilla\SVGSVGElement.webidl. Do not edit!

package js.html.svg;

/**
	The `SVGSVGElement` interface provides access to the properties of `svg` elements, as well as methods to manipulate them. This interface contains also various miscellaneous commonly-used utility methods, such as matrix operations and the ability to control the time of redraw on visual rendering devices.

	Documentation [SVGSVGElement](https://developer.mozilla.org/en-US/docs/Web/API/SVGSVGElement) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/SVGSVGElement$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/SVGSVGElement>
**/
@:native("SVGSVGElement")
extern class SVGElement extends GraphicsElement
{
	static inline var SVG_ZOOMANDPAN_UNKNOWN : Int = 0;
	static inline var SVG_ZOOMANDPAN_DISABLE : Int = 1;
	static inline var SVG_ZOOMANDPAN_MAGNIFY : Int = 2;
	
	var x(default,null) : AnimatedLength;
	var y(default,null) : AnimatedLength;
	var width(default,null) : AnimatedLength;
	var height(default,null) : AnimatedLength;
	var pixelUnitToMillimeterX(default,null) : Float;
	var pixelUnitToMillimeterY(default,null) : Float;
	var screenPixelToMillimeterX(default,null) : Float;
	var screenPixelToMillimeterY(default,null) : Float;
	var useCurrentView(default,null) : Bool;
	var currentScale : Float;
	var currentTranslate(default,null) : Point;
	var viewBox(default,null) : AnimatedRect;
	var preserveAspectRatio(default,null) : AnimatedPreserveAspectRatio;
	var zoomAndPan : Int;
	
	function suspendRedraw( maxWaitMilliseconds : Int ) : Int;
	function unsuspendRedraw( suspendHandleID : Int ) : Void;
	function unsuspendRedrawAll() : Void;
	function forceRedraw() : Void;
	function pauseAnimations() : Void;
	function unpauseAnimations() : Void;
	function animationsPaused() : Bool;
	function getCurrentTime() : Float;
	function setCurrentTime( seconds : Float ) : Void;
	function deselectAll() : Void;
	function createSVGNumber() : Number;
	function createSVGLength() : Length;
	function createSVGAngle() : Angle;
	function createSVGPoint() : Point;
	function createSVGMatrix() : Matrix;
	function createSVGRect() : Rect;
	function createSVGTransform() : Transform;
	function createSVGTransformFromMatrix( matrix : Matrix ) : Transform;
	function getElementById( elementId : String ) : js.html.Element;
}