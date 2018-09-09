/*
 * Copyright (C)2005-2018 Haxe Foundation
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
	The `DOMPointReadOnly` interface specifies the coordinate and perspective fields used by `DOMPoint` to define a 2D or 3D point in a coordinate system.

	Documentation [DOMPointReadOnly](https://developer.mozilla.org/en-US/docs/Web/API/DOMPointReadOnly) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/DOMPointReadOnly$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/DOMPointReadOnly>
**/
@:native("DOMPointReadOnly")
extern class DOMPointReadOnly
{
	static function fromPoint( ?other : DOMPointInit ) : DOMPointReadOnly;
	
	/**
		The point's horizontal coordinate, `x`.
	**/
	var x(default,null) : Float;
	
	/**
		The point's vertical coordinate, `y`.
	**/
	var y(default,null) : Float;
	
	/**
		The point's depth coordinate, `z`.
	**/
	var z(default,null) : Float;
	
	/**
		The point's perspective value, `w`.
	**/
	var w(default,null) : Float;
	
	/** @throws DOMError */
	function new( ?x : Float = 0.0, ?y : Float = 0.0, ?z : Float = 0.0, ?w : Float = 1.0 ) : Void;
	
	/**
		Returns a JSON representation of the `DOMPointReadOnly` object.
	**/
	function toJSON() : Dynamic;
}