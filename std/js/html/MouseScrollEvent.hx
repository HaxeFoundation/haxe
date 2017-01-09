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

// This file is generated from mozilla\MouseScrollEvent.webidl. Do not edit!

package js.html;

/**
	The `MouseScrollEvent` interface represents events that occur due to the user moving a mouse wheel or similar input device.

	Documentation [MouseScrollEvent](https://developer.mozilla.org/en-US/docs/Web/API/MouseScrollEvent) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/MouseScrollEvent$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/MouseScrollEvent>
**/
@:native("MouseScrollEvent")
extern class MouseScrollEvent extends MouseEvent
{
	static inline var HORIZONTAL_AXIS : Int = 1;
	static inline var VERTICAL_AXIS : Int = 2;
	
	var axis(default,null) : Int;
	
	function initMouseScrollEvent( type : String, canBubble : Bool, cancelable : Bool, view : Window, detail : Int, screenX : Int, screenY : Int, clientX : Int, clientY : Int, ctrlKey : Bool, altKey : Bool, shiftKey : Bool, metaKey : Bool, button : Int, relatedTarget : EventTarget, axis : Int ) : Void;
}