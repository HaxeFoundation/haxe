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

// This file is generated from mozilla\MediaQueryListEvent.webidl. Do not edit!

package js.html;

/**
	The `MediaQueryListEvent` object stores information on the changes that have happened to a `MediaQueryList` object — instances are available as the event object on a function referenced by a `MediaQueryList.onchange` property or `MediaQueryList.addEvent()` call.

	Documentation [MediaQueryListEvent](https://developer.mozilla.org/en-US/docs/Web/API/MediaQueryListEvent) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/MediaQueryListEvent$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/MediaQueryListEvent>
**/
@:native("MediaQueryListEvent")
extern class MediaQueryListEvent extends Event {
	
	/**
		 A `DOMString` representing a serialized media query.
	**/
	var media(default,null) : String;
	
	/**
		 A `Boolean` that returns `true` if the `document` currently matches the media query list, or `false` if not.
	**/
	var matches(default,null) : Bool;
	
	/** @throws DOMError */
	function new( type : String, ?eventInitDict : MediaQueryListEventInit ) : Void;
}