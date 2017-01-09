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

// This file is generated from mozilla\PointerEvent.webidl. Do not edit!

package js.html;

/**
	The `PointerEvent` interface represents the state of a DOM event produced by a pointer such as the geometry of the contact point, the device type that generated the event, the amount of pressure that was applied on the contact surface, etc.

	Documentation [PointerEvent](https://developer.mozilla.org/en-US/docs/Web/API/PointerEvent) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/PointerEvent$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/PointerEvent>
**/
@:native("PointerEvent")
extern class PointerEvent extends MouseEvent
{
	var pointerId(default,null) : Int;
	var width(default,null) : Int;
	var height(default,null) : Int;
	var pressure(default,null) : Float;
	var tiltX(default,null) : Int;
	var tiltY(default,null) : Int;
	var pointerType(default,null) : String;
	var isPrimary(default,null) : Bool;
	
	/** @throws DOMError */
	function new( type : String, ?eventInitDict : PointerEventInit ) : Void;
}