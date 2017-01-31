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

// This file is generated from mozilla\Position.webidl. Do not edit!

package js.html;

/**
	The `Position` interface represents the position of the concerned device at a given time. The position, represented by a `Coordinates` object, comprehends the 2D position of the device, on a spheroid representing the Earth, but also its altitude and its speed.

	Documentation [Position](https://developer.mozilla.org/en-US/docs/Web/API/Position) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/Position$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/Position>
**/
@:native("Position")
extern class Position
{
	
	/**
		Returns a `Coordinates` object defining the current location.
	**/
	var coords(default,null) : Coordinates;
	
	/**
		Returns a `DOMTimeStamp` representing the time at which the location was retrieved.
	**/
	var timestamp(default,null) : Int;
	
}