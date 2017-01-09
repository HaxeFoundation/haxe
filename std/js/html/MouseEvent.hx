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

// This file is generated from mozilla\MouseEvent.webidl. Do not edit!

package js.html;

/**
	The `MouseEvent` interface represents events that occur due to the user interacting with a pointing device (such as a mouse). Common events using this interface include `click`, `dblclick`, `mouseup`, `mousedown`.

	Documentation [MouseEvent](https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent>
**/
@:native("MouseEvent")
extern class MouseEvent extends UIEvent
{
	
	/**
		The X coordinate of the mouse pointer in global (screen) coordinates.
	**/
	var screenX(default,null) : Int;
	
	/**
		The Y coordinate of the mouse pointer in global (screen) coordinates.
	**/
	var screenY(default,null) : Int;
	
	/**
		The X coordinate of the mouse pointer in local (DOM content) coordinates.
	**/
	var clientX(default,null) : Int;
	
	/**
		The Y coordinate of the mouse pointer in local (DOM content) coordinates.
	**/
	var clientY(default,null) : Int;
	
	/**
		The X coordinate of the mouse pointer relative to the position of the padding edge of the target node.
	**/
	var offsetX(default,null) : Int;
	
	/**
		The Y coordinate of the mouse pointer relative to the position of the padding edge of the target node.
	**/
	var offsetY(default,null) : Int;
	
	/**
		Returns `true` if the control key was down when the mouse event was fired.
	**/
	var ctrlKey(default,null) : Bool;
	
	/**
		Returns `true` if the shift key was down when the mouse event was fired.
	**/
	var shiftKey(default,null) : Bool;
	
	/**
		Returns `true` if the alt key was down when the mouse event was fired.
	**/
	var altKey(default,null) : Bool;
	
	/**
		Returns `true` if the meta key was down when the mouse event was fired.
	**/
	var metaKey(default,null) : Bool;
	
	/**
		The button number that was pressed when the mouse event was fired. 
	**/
	var button(default,null) : Int;
	
	/**
		
		 The buttons being pressed when the mouse event was fired
		 
	**/
	var buttons(default,null) : Int;
	
	/**
		
		 The secondary target for the event, if there is one.
		 
	**/
	var relatedTarget(default,null) : EventTarget;
	
	/**
		Returns the id of the hit region affected by the event. If no hit region is affected, `null` is returned.
	**/
	var region(default,null) : String;
	
	/**
		The X coordinate of the mouse pointer relative to the position of the last `mousemove` event.
	**/
	var movementX(default,null) : Int;
	
	/**
		The Y coordinate of the mouse pointer relative to the position of the last `mousemove` event.
	**/
	var movementY(default,null) : Int;
	
	/** @throws DOMError */
	function new( typeArg : String, ?mouseEventInitDict : MouseEventInit ) : Void;
	
	/**
		Initializes the value of a `MouseEvent` created. If the event has already being dispatched, this method does nothing.
	**/
	function initMouseEvent( typeArg : String, canBubbleArg : Bool, cancelableArg : Bool, viewArg : Window, detailArg : Int, screenXArg : Int, screenYArg : Int, clientXArg : Int, clientYArg : Int, ctrlKeyArg : Bool, altKeyArg : Bool, shiftKeyArg : Bool, metaKeyArg : Bool, buttonArg : Int, relatedTargetArg : EventTarget ) : Void;
	
	/**
		Returns the current state of the specified modifier key. See the `KeyboardEvent.getModifierState`() for details.
	**/
	function getModifierState( keyArg : String ) : Bool;
	function initNSMouseEvent( typeArg : String, canBubbleArg : Bool, cancelableArg : Bool, viewArg : Window, detailArg : Int, screenXArg : Int, screenYArg : Int, clientXArg : Int, clientYArg : Int, ctrlKeyArg : Bool, altKeyArg : Bool, shiftKeyArg : Bool, metaKeyArg : Bool, buttonArg : Int, relatedTargetArg : EventTarget, pressure : Float, inputSourceArg : Int ) : Void;
}