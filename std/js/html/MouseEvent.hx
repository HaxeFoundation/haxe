/*
 * Copyright (C)2005-2015 Haxe Foundation
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

// This file is generated from mozilla/MouseEvent.webidl line 13:0. Do not edit!

package js.html;

@:native("MouseEvent")
extern class MouseEvent extends UIEvent
{
	var screenX(default,null) : Int;
	var screenY(default,null) : Int;
	var clientX(default,null) : Int;
	var clientY(default,null) : Int;
	var ctrlKey(default,null) : Bool;
	var shiftKey(default,null) : Bool;
	var altKey(default,null) : Bool;
	var metaKey(default,null) : Bool;
	var button(default,null) : Int;
	var buttons(default,null) : Int;
	var relatedTarget(default,null) : EventTarget;
	var region(default,null) : String;
	var movementX(default,null) : Int;
	var movementY(default,null) : Int;
	
	/** @throws DOMError */
	function new( typeArg : String, ?mouseEventInitDict : MouseEventInit ) : Void;
	/** @throws DOMError */
	function initMouseEvent( typeArg : String, canBubbleArg : Bool, cancelableArg : Bool, viewArg : Window, detailArg : Int, screenXArg : Int, screenYArg : Int, clientXArg : Int, clientYArg : Int, ctrlKeyArg : Bool, altKeyArg : Bool, shiftKeyArg : Bool, metaKeyArg : Bool, buttonArg : Int, relatedTargetArg : EventTarget ) : Void;
	function getModifierState( keyArg : String ) : Bool;
	/** @throws DOMError */
	function initNSMouseEvent( typeArg : String, canBubbleArg : Bool, cancelableArg : Bool, viewArg : Window, detailArg : Int, screenXArg : Int, screenYArg : Int, clientXArg : Int, clientYArg : Int, ctrlKeyArg : Bool, altKeyArg : Bool, shiftKeyArg : Bool, metaKeyArg : Bool, buttonArg : Int, relatedTargetArg : EventTarget, pressure : Float, inputSourceArg : Int ) : Void;
}