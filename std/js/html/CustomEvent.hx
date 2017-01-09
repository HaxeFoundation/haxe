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

// This file is generated from mozilla\CustomEvent.webidl. Do not edit!

package js.html;

/**
	This interface inherits properties from its parent, `Event`:

	Documentation [CustomEvent](https://developer.mozilla.org/en-US/docs/Web/API/CustomEvent) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/CustomEvent$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/CustomEvent>
**/
@:native("CustomEvent")
extern class CustomEvent extends Event
{
	
	/**
		Any data passed when initializing the event.
	**/
	var detail(default,null) : Dynamic;
	
	/** @throws DOMError */
	function new( type : String, ?eventInitDict : CustomEventInit ) : Void;
	/** @throws DOMError */
	
	/**
		
		 Initializes a `CustomEvent` object. If the event has already being dispatched, this method does nothing.
		 
	**/
	function initCustomEvent( type : String, canBubble : Bool, cancelable : Bool, detail : Dynamic ) : Void;
}