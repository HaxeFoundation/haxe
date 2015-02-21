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

// This file is generated from mozilla/Event.webidl line 15:0. Do not edit!

package js.html;

@:native("Event")
extern class Event
{
	static inline var NONE : Int = 0;
	static inline var CAPTURING_PHASE : Int = 1;
	static inline var AT_TARGET : Int = 2;
	static inline var BUBBLING_PHASE : Int = 3;
	static inline var ALT_MASK : Int = 1;
	static inline var CONTROL_MASK : Int = 2;
	static inline var SHIFT_MASK : Int = 4;
	static inline var META_MASK : Int = 8;
	
	var type(default,null) : String;
	var target(default,null) : EventTarget;
	var currentTarget(default,null) : EventTarget;
	var eventPhase(default,null) : Int;
	var bubbles(default,null) : Bool;
	var cancelable(default,null) : Bool;
	var defaultPrevented(default,null) : Bool;
	var isTrusted(default,null) : Bool;
	var timeStamp(default,null) : Float;
	var originalTarget(default,null) : EventTarget;
	var explicitOriginalTarget(default,null) : EventTarget;
	
	/** @throws DOMError */
	function new( type : String, ?eventInitDict : EventInit ) : Void;
	function stopPropagation() : Void;
	function stopImmediatePropagation() : Void;
	function preventDefault() : Void;
	/** @throws DOMError */
	function initEvent( type : String, bubbles : Bool, cancelable : Bool ) : Void;
	function getPreventDefault() : Bool;
}