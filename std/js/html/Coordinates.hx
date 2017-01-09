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

// This file is generated from mozilla\Coordinates.webidl. Do not edit!

package js.html;

/**
	The `Coordinates` interface represents the position and altitude of the device on Earth, as well as the accuracy with which these properties are calculated.

	Documentation [Coordinates](https://developer.mozilla.org/en-US/docs/Web/API/Coordinates) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/Coordinates$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/Coordinates>
**/
@:native("Coordinates")
extern class Coordinates
{
	
	/**
		Returns a `double` representing the position's latitude in decimal degrees.
	**/
	var latitude(default,null) : Float;
	
	/**
		Returns a `double` representing the position's longitude in decimal degrees.
	**/
	var longitude(default,null) : Float;
	
	/**
		Returns a `double` representing the position's altitude in metres, relative to sea level. This value can be `null` if the implementation cannot provide the data.
	**/
	var altitude(default,null) : Float;
	
	/**
		Returns a `double` representing the accuracy of the `latitude` and `longitude` properties, expressed in meters.
	**/
	var accuracy(default,null) : Float;
	
	/**
		Returns a `double` representing the accuracy of the `altitude` expressed in meters. This value can be `null`.
	**/
	var altitudeAccuracy(default,null) : Float;
	
	/**
		Returns a `double` representing the direction in which the device is traveling. This value, specified in degrees, indicates how far off from heading true north the device is. `0` degrees represents true north, and the direction is determined clockwise (which means that east is `90` degrees and west is `270` degrees). If `speed` is `0`, `heading` is `NaN`. If the device is unable to provide `heading` information, this value is `null`.
	**/
	var heading(default,null) : Float;
	
	/**
		Returns a `double` representing the velocity of the device in meters per second. This value can be `null`.
	**/
	var speed(default,null) : Float;
	
}