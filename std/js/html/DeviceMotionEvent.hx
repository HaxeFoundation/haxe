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

// This file is generated from mozilla\DeviceMotionEvent.webidl. Do not edit!

package js.html;

/**
	The `DeviceMotionEvent` provides web developers with information about the speed of changes for the device's position and orientation.

	Documentation [DeviceMotionEvent](https://developer.mozilla.org/en-US/docs/Web/API/DeviceMotionEvent) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/DeviceMotionEvent$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/DeviceMotionEvent>
**/
@:native("DeviceMotionEvent")
extern class DeviceMotionEvent extends Event
{
	
	/**
		An object giving the acceleration of the device on the three axis X, Y and Z. Acceleration is expressed in m/s2.
	**/
	var acceleration(default,null) : DeviceAcceleration;
	
	/**
		An object giving the acceleration of the device on the three axis X, Y and Z with the effect of gravity. Acceleration is expressed in m/s2.
	**/
	var accelerationIncludingGravity(default,null) : DeviceAcceleration;
	
	/**
		An object giving the rate of change of the device's orientation on the three orientation axis alpha, beta and gamma. Rotation rate is express in degrees per seconds.
	**/
	var rotationRate(default,null) : DeviceRotationRate;
	
	/**
		A number representing the interval of time, in milliseconds, at which data is obtained from the device.
	**/
	var interval(default,null) : Float;
	
	/** @throws DOMError */
	function new( type : String, ?eventInitDict : DeviceMotionEventInit ) : Void;
	function initDeviceMotionEvent( type : String, canBubble : Bool, cancelable : Bool, acceleration : DeviceAccelerationInit, accelerationIncludingGravity : DeviceAccelerationInit, rotationRate : DeviceRotationRateInit, interval : Float ) : Void;
}