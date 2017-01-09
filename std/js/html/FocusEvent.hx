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

// This file is generated from mozilla\FocusEvent.webidl. Do not edit!

package js.html;

/**
	The `FocusEvent` interface represents focus-related events like `focus`, `blur`, `focusin`, or `focusout`.

	Documentation [FocusEvent](https://developer.mozilla.org/en-US/docs/Web/API/FocusEvent) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/FocusEvent$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/FocusEvent>
**/
@:native("FocusEvent")
extern class FocusEvent extends UIEvent
{
	
	/**
		Is an `EventTarget` representing a secondary target for this event. As in some cases (like when tabbing in or out a page), this property may be set to `null` for security reasons.
	**/
	var relatedTarget(default,null) : EventTarget;
	
	/** @throws DOMError */
	function new( typeArg : String, ?focusEventInitDict : FocusEventInit ) : Void;
}