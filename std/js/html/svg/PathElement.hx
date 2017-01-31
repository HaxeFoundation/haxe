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

// This file is generated from mozilla\SVGPathElement.webidl. Do not edit!

package js.html.svg;

/**
	The `SVGPathElement` interface corresponds to the `path` element.

	Documentation [SVGPathElement](https://developer.mozilla.org/en-US/docs/Web/API/SVGPathElement) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/SVGPathElement$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/SVGPathElement>
**/
@:native("SVGPathElement")
extern class PathElement extends GraphicsElement
{
	var pathLength(default,null) : AnimatedNumber;
	var pathSegList(default,null) : PathSegList;
	var animatedPathSegList(default,null) : PathSegList;
	
	function getTotalLength() : Float;
	/** @throws DOMError */
	function getPointAtLength( distance : Float ) : Point;
	function getPathSegAtLength( distance : Float ) : Int;
	function createSVGPathSegClosePath() : PathSegClosePath;
	function createSVGPathSegMovetoAbs( x : Float, y : Float ) : PathSegMovetoAbs;
	function createSVGPathSegMovetoRel( x : Float, y : Float ) : PathSegMovetoRel;
	function createSVGPathSegLinetoAbs( x : Float, y : Float ) : PathSegLinetoAbs;
	function createSVGPathSegLinetoRel( x : Float, y : Float ) : PathSegLinetoRel;
	function createSVGPathSegCurvetoCubicAbs( x : Float, y : Float, x1 : Float, y1 : Float, x2 : Float, y2 : Float ) : PathSegCurvetoCubicAbs;
	function createSVGPathSegCurvetoCubicRel( x : Float, y : Float, x1 : Float, y1 : Float, x2 : Float, y2 : Float ) : PathSegCurvetoCubicRel;
	function createSVGPathSegCurvetoQuadraticAbs( x : Float, y : Float, x1 : Float, y1 : Float ) : PathSegCurvetoQuadraticAbs;
	function createSVGPathSegCurvetoQuadraticRel( x : Float, y : Float, x1 : Float, y1 : Float ) : PathSegCurvetoQuadraticRel;
	function createSVGPathSegArcAbs( x : Float, y : Float, r1 : Float, r2 : Float, angle : Float, largeArcFlag : Bool, sweepFlag : Bool ) : PathSegArcAbs;
	function createSVGPathSegArcRel( x : Float, y : Float, r1 : Float, r2 : Float, angle : Float, largeArcFlag : Bool, sweepFlag : Bool ) : PathSegArcRel;
	function createSVGPathSegLinetoHorizontalAbs( x : Float ) : PathSegLinetoHorizontalAbs;
	function createSVGPathSegLinetoHorizontalRel( x : Float ) : PathSegLinetoHorizontalRel;
	function createSVGPathSegLinetoVerticalAbs( y : Float ) : PathSegLinetoVerticalAbs;
	function createSVGPathSegLinetoVerticalRel( y : Float ) : PathSegLinetoVerticalRel;
	function createSVGPathSegCurvetoCubicSmoothAbs( x : Float, y : Float, x2 : Float, y2 : Float ) : PathSegCurvetoCubicSmoothAbs;
	function createSVGPathSegCurvetoCubicSmoothRel( x : Float, y : Float, x2 : Float, y2 : Float ) : PathSegCurvetoCubicSmoothRel;
	function createSVGPathSegCurvetoQuadraticSmoothAbs( x : Float, y : Float ) : PathSegCurvetoQuadraticSmoothAbs;
	function createSVGPathSegCurvetoQuadraticSmoothRel( x : Float, y : Float ) : PathSegCurvetoQuadraticSmoothRel;
}