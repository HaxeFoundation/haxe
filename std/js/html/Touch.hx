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

// This file is generated from mozilla\Touch.webidl. Do not edit!

package js.html;

/**
	The `Touch` interface represents a single contact point on a touch-sensitive device. The contact point is commonly a finger or stylus and the device may be a touchscreen or trackpad.

	Documentation [Touch](https://developer.mozilla.org/en-US/docs/Web/API/Touch) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/Touch$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/Touch>
**/
@:native("Touch")
extern class Touch
{
	var identifier(default,null) : Int;
	var target(default,null) : EventTarget;
	var screenX(default,null) : Int;
	var screenY(default,null) : Int;
	var clientX(default,null) : Int;
	var clientY(default,null) : Int;
	var pageX(default,null) : Int;
	var pageY(default,null) : Int;
	var radiusX(default,null) : Int;
	var radiusY(default,null) : Int;
	var rotationAngle(default,null) : Float;
	var force(default,null) : Float;
	
	/** @throws DOMError */
	function new( touchInitDict : TouchInit ) : Void;
}