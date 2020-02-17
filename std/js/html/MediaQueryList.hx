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

// This file is generated from mozilla\MediaQueryList.webidl. Do not edit!

package js.html;

/**
	A `MediaQueryList` object stores information on a media query applied to a document, and handles sending notifications to listeners when the media query state change (i.e. when the media query test starts or stops evaluating to `true`).

	Documentation [MediaQueryList](https://developer.mozilla.org/en-US/docs/Web/API/MediaQueryList) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/MediaQueryList$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/MediaQueryList>
**/
@:native("MediaQueryList")
extern class MediaQueryList extends EventTarget {
	
	/**
		 A `DOMString` representing a serialized media query.
	**/
	var media(default,null) : String;
	
	/**
		 A `Boolean` that returns `true` if the `document` currently matches the media query list, or `false` if not.
	**/
	var matches(default,null) : Bool;
	
	/**
		 An event handler property representing a function that is invoked when the `change` event fires, i.e when the status of media query support changes. The event object is a `MediaQueryListEvent` instance, which is recognised as a `MediaListQuery` instance in older browsers, for backwards compatibility purposes.
	**/
	var onchange : haxe.Constraints.Function;
	
	
	/**
		 Adds a listener to the `MediaQueryListener` that will run a custom callback function in response to the media query status changing. This is basically an alias for `EventTarget.addEventListener()`, for backwards compatibility purposes.
		@throws DOMError
	**/
	@:overload( function( listener : haxe.Constraints.Function) : Void {} )
	@:overload( function( listener : EventListener) : Void {} )
	function addListener( listener : Event -> Void ) : Void;
	
	/**
		 Removes a listener from the `MediaQueryListener`. This is basically an alias for `EventTarget.removeEventListener()`, for backwards compatibility purposes.
		@throws DOMError
	**/
	@:overload( function( listener : haxe.Constraints.Function) : Void {} )
	@:overload( function( listener : EventListener) : Void {} )
	function removeListener( listener : Event -> Void ) : Void;
}