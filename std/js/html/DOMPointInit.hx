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

// This file is generated from mozilla\DOMPoint.webidl. Do not edit!

package js.html;

/**
	The `DOMPointInit` dictionary is used to provide the values of the coordinates and perspective when creating and JSONifying a `DOMPoint` or `DOMPointReadOnly` object.

	Documentation [DOMPointInit](https://developer.mozilla.org/en-US/docs/Web/API/DOMPointInit) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/DOMPointInit$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/DOMPointInit>
**/
typedef DOMPointInit = {
	
	/**
		The point's w perspective value given as an unrestricted floating-point number. The default is 1.
	**/
	var ?w : Float;
	
	/**
		An unrestricted floating-point value indicating the x-coordinate of the point in space. This is generally the horizontal coordinate, with positive values being to the right and negative values to the left. The default value is 0.
	**/
	var ?x : Float;
	
	/**
		An unrestricted floating-point number providing the point's y-coordinate. This is the vertical coordinate, and barring any transforms applied to the coordinate system, positive values are downward and negative values upward toward the top of the screen. The default is 0.
	**/
	var ?y : Float;
	
	/**
		An unrestricted floating-point value which gives the point's z-coordinate, which is (assuming no transformations that alter the situation) the depth coordinate; positive values are closer to the user and negative values retreat back into the screen. The default value is 0.
	**/
	var ?z : Float;
}