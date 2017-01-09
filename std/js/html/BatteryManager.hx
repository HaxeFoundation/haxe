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

// This file is generated from mozilla\BatteryManager.webidl. Do not edit!

package js.html;

/**
	The `BatteryManager` interface provides ways to get information about the system's battery charge level.

	Documentation [BatteryManager](https://developer.mozilla.org/en-US/docs/Web/API/BatteryManager) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/BatteryManager$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/BatteryManager>
**/
@:native("BatteryManager")
extern class BatteryManager extends EventTarget
{
	
	/**
		A Boolean value indicating whether or not the battery is currently being charged.
	**/
	var charging(default,null) : Bool;
	
	/**
		A number representing the remaining time in seconds until the battery is fully charged, or 0 if the battery is already fully charged.
	**/
	var chargingTime(default,null) : Float;
	
	/**
		A number representing the remaining time in seconds until the battery is completely discharged and the system will suspend.
	**/
	var dischargingTime(default,null) : Float;
	
	/**
		A number representing the system's battery charge level scaled to a value between 0.0 and 1.0.
	**/
	var level(default,null) : Float;
	
	/**
		A handler for the `chargingchange` event; This event is sent when the battery charging state is updated.
	**/
	var onchargingchange : haxe.Constraints.Function;
	
	/**
		A handler for the `chargingtimechange` event; This event is sent when the battery charging time is updated
	**/
	var onchargingtimechange : haxe.Constraints.Function;
	
	/**
		A handler for the `dischargingtimechange` event; This event is sent when the battery discharging time is updated.
	**/
	var ondischargingtimechange : haxe.Constraints.Function;
	
	/**
		A handler for the `levelchange` event; This event is sent when the battery level is updated.
	**/
	var onlevelchange : haxe.Constraints.Function;
	
}