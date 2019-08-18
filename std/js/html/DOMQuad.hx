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

// This file is generated from mozilla\DOMQuad.webidl. Do not edit!

package js.html;

/**
	A `DOMQuad` is a collection of four `DOMPoint`s defining the corners of an arbitrary quadrilateral. Returning `DOMQuad`s lets `getBoxQuads()` return accurate information even when arbitrary 2D or 3D transforms are present. It has a handy `bounds` attribute returning a `DOMRectReadOnly` for those cases where you just want an axis-aligned bounding rectangle.

	Documentation [DOMQuad](https://developer.mozilla.org/en-US/docs/Web/API/DOMQuad) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/DOMQuad$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/DOMQuad>
**/
@:native("DOMQuad")
extern class DOMQuad {
	
	/**
		are `DOMPoint` objects for each of the `DOMQuad` object's four corners.
	**/
	var p1(default,null) : DOMPoint;
	var p2(default,null) : DOMPoint;
	var p3(default,null) : DOMPoint;
	var p4(default,null) : DOMPoint;
	var bounds(default,null) : DOMRectReadOnly;
	
	/** @throws DOMError */
	@:overload( function( ?p1 : DOMPointInit, ?p2 : DOMPointInit, ?p3 : DOMPointInit, ?p4 : DOMPointInit ) : Void {} )
	function new( rect : DOMRectReadOnly ) : Void;
	
	/**
		Returns a `DOMRect` object with the coordinates and dimensions of the `DOMQuad` object.
	**/
	function getBounds() : DOMRectReadOnly;
	
	/**
		Returns a JSON representation of the `DOMQuad` object.
	**/
	function toJSON() : DOMQuadJSON;
}