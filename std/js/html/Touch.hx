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

// This file is generated from mozilla\Touch.webidl. Do not edit!

package js.html;

/**
	The `Touch` interface represents a single contact point on a touch-sensitive device. The contact point is commonly a finger or stylus and the device may be a touchscreen or trackpad.

	Documentation [Touch](https://developer.mozilla.org/en-US/docs/Web/API/Touch) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/Touch$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/Touch>
**/
@:native("Touch")
extern class Touch {
	
	/**
		Returns a unique identifier for this `Touch` object. A given touch point (say, by a finger) will have the same identifier for the duration of its movement around the surface. This lets you ensure that you're tracking the same touch all the time.
	**/
	var identifier(default,null) : Int;
	
	/**
		Returns the `Element` on which the touch point started when it was first placed on the surface, even if the touch point has since moved outside the interactive area of that element or even been removed from the document.
	**/
	var target(default,null) : EventTarget;
	
	/**
		Returns the X coordinate of the touch point relative to the left edge of the screen.
	**/
	var screenX(default,null) : Int;
	
	/**
		Returns the Y coordinate of the touch point relative to the top edge of the screen.
	**/
	var screenY(default,null) : Int;
	
	/**
		Returns the X coordinate of the touch point relative to the left edge of the browser viewport, not including any scroll offset.
	**/
	var clientX(default,null) : Int;
	
	/**
		Returns the Y coordinate of the touch point relative to the top edge of the browser viewport, not including any scroll offset.
	**/
	var clientY(default,null) : Int;
	
	/**
		Returns the X coordinate of the touch point relative to the left edge of the document. Unlike `clientX`, this value includes the horizontal scroll offset, if any.
	**/
	var pageX(default,null) : Int;
	
	/**
		Returns the Y coordinate of the touch point relative to the top of the document. Unlike `clientY,` this value includes the vertical scroll offset, if any.
	**/
	var pageY(default,null) : Int;
	
	/**
		Returns the X radius of the ellipse that most closely circumscribes the area of contact with the screen. The value is in pixels of the same scale as `screenX`.
	**/
	var radiusX(default,null) : Int;
	
	/**
		Returns the Y radius of the ellipse that most closely circumscribes the area of contact with the screen. The value is in pixels of the same scale as `screenY`.
	**/
	var radiusY(default,null) : Int;
	
	/**
		Returns the angle (in degrees) that the ellipse described by radiusX and radiusY must be rotated, clockwise, to most accurately cover the area of contact between the user and the surface.
	**/
	var rotationAngle(default,null) : Float;
	
	/**
		Returns the amount of pressure being applied to the surface by the user, as a `float` between `0.0` (no pressure) and `1.0` (maximum pressure).
	**/
	var force(default,null) : Float;
	
	/** @throws DOMError */
	function new( touchInitDict : TouchInit ) : Void;
}