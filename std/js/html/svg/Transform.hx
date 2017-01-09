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

// This file is generated from mozilla\SVGTransform.webidl. Do not edit!

package js.html.svg;

/**
	`SVGTransform` is the interface for one of the component transformations within an `SVGTransformList`; thus, an `SVGTransform` object corresponds to a single component (e.g., `scale(…)` or `matrix(…)`) within a `transform` attribute.

	Documentation [SVGTransform](https://developer.mozilla.org/en-US/docs/Web/API/SVGTransform) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/SVGTransform$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/SVGTransform>
**/
@:native("SVGTransform")
extern class Transform
{
	static inline var SVG_TRANSFORM_UNKNOWN : Int = 0;
	static inline var SVG_TRANSFORM_MATRIX : Int = 1;
	static inline var SVG_TRANSFORM_TRANSLATE : Int = 2;
	static inline var SVG_TRANSFORM_SCALE : Int = 3;
	static inline var SVG_TRANSFORM_ROTATE : Int = 4;
	static inline var SVG_TRANSFORM_SKEWX : Int = 5;
	static inline var SVG_TRANSFORM_SKEWY : Int = 6;
	
	var type(default,null) : Int;
	var matrix(default,null) : Matrix;
	var angle(default,null) : Float;
	
	/** @throws DOMError */
	function setMatrix( matrix : Matrix ) : Void;
	/** @throws DOMError */
	function setTranslate( tx : Float, ty : Float ) : Void;
	/** @throws DOMError */
	function setScale( sx : Float, sy : Float ) : Void;
	/** @throws DOMError */
	function setRotate( angle : Float, cx : Float, cy : Float ) : Void;
	/** @throws DOMError */
	function setSkewX( angle : Float ) : Void;
	/** @throws DOMError */
	function setSkewY( angle : Float ) : Void;
}