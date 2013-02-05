/*
 * Copyright (C)2005-2013 Haxe Foundation
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

// This file is generated, do not edit!
package js.html;

/** <p>This chapter describes the DOM Event Model. The <a class="external" rel="external" href="http://www.w3.org/TR/DOM-Level-2-Events/events.html#Events-Event" title="http://www.w3.org/TR/DOM-Level-2-Events/events.html#Events-Event" target="_blank">Event</a> interface itself is described, as well as the interfaces for event registration on nodes in the DOM, and <a title="en/DOM/element.addEventListener" rel="internal" href="https://developer.mozilla.org/en/DOM/element.addEventListener">event listeners</a>, and several longer examples that show how the various event interfaces relate to one another.</p>
<p>There is an excellent diagram that clearly explains the three phases of event flow through the DOM in the <a class="external" title="http://www.w3.org/TR/DOM-Level-3-Events/#dom-event-architecture" rel="external" href="http://www.w3.org/TR/DOM-Level-3-Events/#dom-event-architecture" target="_blank">DOM Level 3 Events draft</a>.</p><br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/event">MDN</a>. */
@:native("Event")
extern class Event
{
	static inline var AT_TARGET : Int = 2;

	static inline var BLUR : Int = 8192;

	static inline var BUBBLING_PHASE : Int = 3;

	static inline var CAPTURING_PHASE : Int = 1;

	static inline var CHANGE : Int = 32768;

	static inline var CLICK : Int = 64;

	static inline var DBLCLICK : Int = 128;

	static inline var DRAGDROP : Int = 2048;

	static inline var FOCUS : Int = 4096;

	static inline var KEYDOWN : Int = 256;

	static inline var KEYPRESS : Int = 1024;

	static inline var KEYUP : Int = 512;

	static inline var MOUSEDOWN : Int = 1;

	static inline var MOUSEDRAG : Int = 32;

	static inline var MOUSEMOVE : Int = 16;

	static inline var MOUSEOUT : Int = 8;

	static inline var MOUSEOVER : Int = 4;

	static inline var MOUSEUP : Int = 2;

	static inline var NONE : Int = 0;

	static inline var SELECT : Int = 16384;

	/** A boolean indicating whether the event bubbles up through the DOM or not. */
	var bubbles(default,null) : Bool;

	/** A boolean indicating whether the bubbling of the event has been canceled or not. */
	var cancelBubble : Bool;

	/** A boolean indicating whether the event is cancelable. */
	var cancelable(default,null) : Bool;

	var clipboardData(default,null) : Clipboard;

	/** A reference to the currently registered target for the event. */
	var currentTarget(default,null) : EventTarget;

	/** Indicates whether or not <code><a rel="custom" href="https://developer.mozilla.org/en/DOM/event.preventDefault">event.preventDefault()</a></code>
 has been called on the event. */
	var defaultPrevented(default,null) : Bool;

	/** Indicates which phase of the event flow is being processed. */
	var eventPhase(default,null) : Int;

	var returnValue : Bool;

	var srcElement(default,null) : EventTarget;

	/** A reference to the target to which the event was originally dispatched. */
	var target(default,null) : EventTarget;

	/** The time that the event was created. */
	var timeStamp(default,null) : Int;

	/** The name of the event (case-insensitive). */
	var type(default,null) : String;

	function new(type : String, canBubble : Bool = true, cancelable : Bool = true) : Void;

	function initEvent( eventTypeArg : String, canBubbleArg : Bool, cancelableArg : Bool ) : Void;

	function preventDefault() : Void;

	function stopImmediatePropagation() : Void;

	function stopPropagation() : Void;

}
