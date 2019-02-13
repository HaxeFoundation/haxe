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

// This file is generated from mozilla\TouchEvent.webidl. Do not edit!

package js.html;

/**
	The `TouchEvent` interface represents an event sent when the state of contacts with a touch-sensitive surface changes. This surface can be a touch screen or trackpad, for example. The event can describe one or more points of contact with the screen and includes support for detecting movement, addition and removal of contact points, and so forth.

	Documentation [TouchEvent](https://developer.mozilla.org/en-US/docs/Web/API/TouchEvent) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/TouchEvent$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/TouchEvent>
**/
@:native("TouchEvent")
extern class TouchEvent extends UIEvent {
	
	/**
		A `TouchList` of all the `Touch` objects representing all current points of contact with the surface, regardless of target or changed status.
	**/
	var touches(default,null) : TouchList;
	
	/**
		A `TouchList` of all the `Touch` objects that are both currently in contact with the touch surface and were also started on the same element that is the target of the event.
	**/
	var targetTouches(default,null) : TouchList;
	
	/**
		A `TouchList` of all the `Touch` objects representing individual points of contact whose states changed between the previous touch event and this one.
	**/
	var changedTouches(default,null) : TouchList;
	
	/**
		A Boolean value indicating whether or not the alt key was down when the touch event was fired.
	**/
	var altKey(default,null) : Bool;
	
	/**
		A Boolean value indicating whether or not the meta key was down when the touch event was fired.
	**/
	var metaKey(default,null) : Bool;
	
	/**
		A Boolean value indicating whether or not the control key was down when the touch event was fired.
	**/
	var ctrlKey(default,null) : Bool;
	
	/**
		A Boolean value indicating whether or not the shift key was down when the touch event was fired.
	**/
	var shiftKey(default,null) : Bool;
	
	/** @throws DOMError */
	function new( type : String, ?eventInitDict : TouchEventInit ) : Void;
	function initTouchEvent( type : String, canBubble : Bool = false, cancelable : Bool = false, ?view : Window, detail : Int = 0, ctrlKey : Bool = false, altKey : Bool = false, shiftKey : Bool = false, metaKey : Bool = false, ?touches : TouchList, ?targetTouches : TouchList, ?changedTouches : TouchList ) : Void;
}