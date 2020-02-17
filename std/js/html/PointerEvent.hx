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

// This file is generated from mozilla\PointerEvent.webidl. Do not edit!

package js.html;

/**
	The `PointerEvent` interface represents the state of a DOM event produced by a pointer such as the geometry of the contact point, the device type that generated the event, the amount of pressure that was applied on the contact surface, etc.

	Documentation [PointerEvent](https://developer.mozilla.org/en-US/docs/Web/API/PointerEvent) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/PointerEvent$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/PointerEvent>
**/
@:native("PointerEvent")
extern class PointerEvent extends MouseEvent {
	
	/**
		A unique identifier for the pointer causing the event.
	**/
	var pointerId(default,null) : Int;
	
	/**
		The width (magnitude on the X axis), in CSS pixels, of the contact geometry of the pointer.
	**/
	var width(default,null) : Int;
	
	/**
		The height (magnitude on the Y axis), in CSS pixels, of the contact geometry of the pointer.
	**/
	var height(default,null) : Int;
	
	/**
		The normalized pressure of the pointer input in the range 0 to 1, where 0 and 1 represent the minimum and maximum pressure the hardware is capable of detecting, respectively.
	**/
	var pressure(default,null) : Float;
	
	/**
		The normalized tangential pressure of the pointer input (also known as barrel pressure or cylinder stress) in the range -1 to 1, where 0 is the neutral position of the control.
	**/
	var tangentialPressure(default,null) : Float;
	
	/**
		The plane angle (in degrees, in the range of -90 to 90) between the Y-Z plane and the plane containing both the transducer (e.g. pen stylus) axis and the Y axis.
	**/
	var tiltX(default,null) : Int;
	
	/**
		The plane angle (in degrees, in the range of -90 to 90) between the X-Z plane and the plane containing both the transducer (e.g. pen stylus) axis and the X axis.
	**/
	var tiltY(default,null) : Int;
	
	/**
		The clockwise rotation of the transducer (e.g. pen stylus) around its major axis in degrees, with a value in the range 0 to 359.
	**/
	var twist(default,null) : Int;
	
	/**
		Indicates the device type that caused the event (mouse, pen, touch, etc.)
	**/
	var pointerType(default,null) : String;
	
	/**
		Indicates if the pointer represents the primary pointer of this pointer type.
	**/
	var isPrimary(default,null) : Bool;
	
	/** @throws DOMError */
	function new( type : String, ?eventInitDict : PointerEventInit ) : Void;
	
	/**
		Returns a sequence of all `PointerEvent` instances that were coalesced into the dispatched `pointermove` event.
	**/
	function getCoalescedEvents() : Array<PointerEvent>;
}