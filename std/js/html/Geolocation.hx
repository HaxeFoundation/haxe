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

// This file is generated from mozilla\Geolocation.webidl. Do not edit!

package js.html;

/**
	The `Geolocation` interface represents an object able to programmatically obtain the position of the device. It gives Web content access to the location of the device. This allows a Web site or app to offer customized results based on the user's location.

	Documentation [Geolocation](https://developer.mozilla.org/en-US/docs/Web/API/Geolocation) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/Geolocation$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/Geolocation>
**/
@:native("Geolocation")
extern class Geolocation
{
	/** @throws DOMError */
	
	/**
		Determines the device's current location and gives back a `Position` object with the data.
	**/
	function getCurrentPosition( successCallback : Position -> Void, ?errorCallback : PositionError -> Void, ?options : PositionOptions ) : Void;
	/** @throws DOMError */
	
	/**
		Returns a `long` value representing the newly established callback function to be invoked whenever the device location changes.
	**/
	function watchPosition( successCallback : Position -> Void, ?errorCallback : PositionError -> Void, ?options : PositionOptions ) : Int;
	
	/**
		Removes the particular handler previously installed using `watchPosition()`.
	**/
	function clearWatch( watchId : Int ) : Void;
}