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

// This file is generated from mozilla\SVGGraphicsElement.webidl. Do not edit!

package js.html.svg;

/**
	The `SVGGraphicsElement` interface represents SVG elements whose primary purpose is to directly render graphics into a group.

	Documentation [SVGGraphicsElement](https://developer.mozilla.org/en-US/docs/Web/API/SVGGraphicsElement) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/SVGGraphicsElement$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/SVGGraphicsElement>
**/
@:native("SVGGraphicsElement")
extern class GraphicsElement extends Element {
	
	/**
		An `SVGAnimatedTransformList` reflecting the computed value of the `transform` property and its corresponding `transform` attribute of the given element.
	**/
	var transform(default,null) : AnimatedTransformList;
	var nearestViewportElement(default,null) : Element;
	var farthestViewportElement(default,null) : Element;
	var requiredFeatures(default,null) : StringList;
	var requiredExtensions(default,null) : StringList;
	var systemLanguage(default,null) : StringList;
	
	
	/**
		Returns a `DOMRect` representing the computed bounding box of the current element.
		@throws DOMError
	**/
	function getBBox( ?aOptions : BoundingBoxOptions ) : Rect;
	
	/**
		Returns a `DOMMatrix` representing the matrix that transforms the current element's coordinate system to its SVG viewport's coordinate system.
	**/
	function getCTM() : Matrix;
	
	/**
		Returns a `DOMMatrix` representing the matrix that transforms the current element's coordinate system to the coordinate system of the SVG viewport for the SVG document fragment.
	**/
	function getScreenCTM() : Matrix;
	/** @throws DOMError */
	function getTransformToElement( element : GraphicsElement ) : Matrix;
	function hasExtension( extension : String ) : Bool;
}