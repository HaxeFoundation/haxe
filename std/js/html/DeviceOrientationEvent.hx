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

// This file is generated from mozilla\DeviceOrientationEvent.webidl. Do not edit!

package js.html;

/**
	The `DeviceOrientationEvent` provides web developers with information from the physical orientation of the device running the web page.

	Documentation [DeviceOrientationEvent](https://developer.mozilla.org/en-US/docs/Web/API/DeviceOrientationEvent) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/DeviceOrientationEvent$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/DeviceOrientationEvent>
**/
@:native("DeviceOrientationEvent")
extern class DeviceOrientationEvent extends Event
{
	
	/**
		A number representing the motion of the device around the z axis, express in degrees with values ranging from 0 to 360
	**/
	var alpha(default,null) : Float;
	
	/**
		A number representing the motion of the device around the x axis, express in degrees with values ranging from -180 to 180. This represents a front to back motion of the device.
	**/
	var beta(default,null) : Float;
	
	/**
		A number representing the motion of the device around the y axis, express in degrees with values ranging from -90 to 90. This represents a left to right motion of the device.
	**/
	var gamma(default,null) : Float;
	
	/**
		A boolean that indicates whether or not the device is providing orientation data absolutely.
	**/
	var absolute(default,null) : Bool;
	
	/** @throws DOMError */
	function new( type : String, ?eventInitDict : DeviceOrientationEventInit ) : Void;
	function initDeviceOrientationEvent( type : String, canBubble : Bool, cancelable : Bool, alpha : Float, beta : Float, gamma : Float, absolute : Bool ) : Void;
}