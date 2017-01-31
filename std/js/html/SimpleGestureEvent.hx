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

// This file is generated from mozilla\SimpleGestureEvent.webidl. Do not edit!

package js.html;

@:native("SimpleGestureEvent")
extern class SimpleGestureEvent extends MouseEvent
{
	static inline var DIRECTION_UP : Int = 1;
	static inline var DIRECTION_DOWN : Int = 2;
	static inline var DIRECTION_LEFT : Int = 4;
	static inline var DIRECTION_RIGHT : Int = 8;
	static inline var ROTATION_COUNTERCLOCKWISE : Int = 1;
	static inline var ROTATION_CLOCKWISE : Int = 2;
	
	var allowedDirections : Int;
	var direction(default,null) : Int;
	var delta(default,null) : Float;
	var clickCount(default,null) : Int;
	
	function initSimpleGestureEvent( typeArg : String, canBubbleArg : Bool, cancelableArg : Bool, viewArg : Window, detailArg : Int, screenXArg : Int, screenYArg : Int, clientXArg : Int, clientYArg : Int, ctrlKeyArg : Bool, altKeyArg : Bool, shiftKeyArg : Bool, metaKeyArg : Bool, buttonArg : Int, relatedTargetArg : EventTarget, allowedDirectionsArg : Int, directionArg : Int, deltaArg : Float, clickCount : Int ) : Void;
}