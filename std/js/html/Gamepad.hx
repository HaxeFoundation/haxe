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

// This file is generated from mozilla\Gamepad.webidl. Do not edit!

package js.html;

/**
	The `Gamepad` interface of the Gamepad API defines an individual gamepad or other controller, allowing access to information such as button presses, axis positions, and id.

	Documentation [Gamepad](https://developer.mozilla.org/en-US/docs/Web/API/Gamepad) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/Gamepad$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/Gamepad>
**/
@:native("Gamepad")
extern class Gamepad
{
	
	/**
		A `DOMString` containing identifying information about the controller.
	**/
	var id(default,null) : String;
	
	/**
		An integer that is auto-incremented to be unique for each device currently connected to the system.
	**/
	var index(default,null) : Int;
	
	/**
		A string indicating whether the browser has remapped the controls on the device to a known layout.
	**/
	var mapping(default,null) : GamepadMappingType;
	
	/**
		A boolean indicating whether the gamepad is still connected to the system.
	**/
	var connected(default,null) : Bool;
	
	/**
		An array of `gamepadButton` objects representing the buttons present on the device.
	**/
	var buttons(default,null) : Array<GamepadButton>;
	
	/**
		An array representing the controls with axes present on the device (e.g. analog thumb sticks).
	**/
	var axes(default,null) : Array<Float>;
	
	/**
		A `DOMHighResTimeStamp` representing the last time the data for this gamepad was updated. Note that this property is not currently supported anywhere.
	**/
	var timestamp(default,null) : Float;
	
}