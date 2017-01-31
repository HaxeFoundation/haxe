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

// This file is generated from mozilla\UIEvent.webidl. Do not edit!

package js.html;

/**
	The `UIEvent` interface represents simple user interface events.

	Documentation [UIEvent](https://developer.mozilla.org/en-US/docs/Web/API/UIEvent) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/UIEvent$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/UIEvent>
**/
@:native("UIEvent")
extern class UIEvent extends Event
{
	static inline var SCROLL_PAGE_UP : Int = -32768;
	static inline var SCROLL_PAGE_DOWN : Int = 32768;
	
	
	/**
		Returns a `WindowProxy` that contains the view that generated the event.
	**/
	var view(default,null) : Window;
	
	/**
		Returns a `long` with details about the event, depending on the event type.
	**/
	var detail(default,null) : Int;
	
	/**
		Returns the horizontal coordinate of the event relative to the current layer.
	**/
	var layerX(default,null) : Int;
	
	/**
		Returns the vertical coordinate of the event relative to the current layer.
	**/
	var layerY(default,null) : Int;
	
	/**
		Returns the horizontal coordinate of the event relative to the whole document.
	**/
	var pageX(default,null) : Int;
	
	/**
		Returns the vertical coordinate of the event relative to the whole document.
	**/
	var pageY(default,null) : Int;
	
	/**
		Returns the numeric `keyCode` of the key pressed, or the character code (`charCode`) for an alphanumeric key pressed.
	**/
	var which(default,null) : Int;
	var rangeParent(default,null) : Node;
	var rangeOffset(default,null) : Int;
	
	/**
		Is a `Boolean` indicating whether the bubbling of the event has been canceled or not.
	**/
	var cancelBubble : Bool;
	
	/**
		Returns a `Boolean` indicating whether the event produced a key character or not.
	**/
	var isChar(default,null) : Bool;
	
	/** @throws DOMError */
	function new( type : String, ?eventInitDict : UIEventInit ) : Void;
	
	/**
		Initializes a `UIEvent` object. If the event has already being dispatched, this method does nothing.
	**/
	function initUIEvent( aType : String, aCanBubble : Bool, aCancelable : Bool, aView : Window, aDetail : Int ) : Void;
}