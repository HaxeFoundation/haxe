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

// This file is generated from mozilla\VisualViewport.webidl. Do not edit!

package js.html;

/**
	The `VisualViewport` interface of the the Visual Viewport API represents the visual viewport for a given window. For a page containing iframes, each iframe, as well as the containing page, will have a unique window object. Each window on a page will have a unique `VisualViewport` representing the properties associated with that window.

	Documentation [VisualViewport](https://developer.mozilla.org/en-US/docs/Web/API/VisualViewport) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/VisualViewport$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/VisualViewport>
**/
@:native("VisualViewport")
extern class VisualViewport extends EventTarget {
	
	/**
		Returns the offset of the left edge of the visual viewport from the left edge of the layout viewport in CSS pixels.
	**/
	var offsetLeft(default,null) : Float;
	
	/**
		Returns the offset of the top edge of the visual viewport from the top edge of the layout viewport in CSS pixels.
	**/
	var offsetTop(default,null) : Float;
	
	/**
		Returns the x coordinate relative to the initial containing block origin of the top edge of the visual viewport in CSS pixels.
	**/
	var pageLeft(default,null) : Float;
	
	/**
		Returns the y coordinate relative to the initial containing block origin of the top edge of the visual viewport in CSS pixels.
	**/
	var pageTop(default,null) : Float;
	
	/**
		Returns the width of the visual viewport in CSS pixels.
	**/
	var width(default,null) : Float;
	
	/**
		Returns the height of the visual viewport in CSS pixels.
	**/
	var height(default,null) : Float;
	
	/**
		Returns the pinch-zoom scaling factor applied to the visual viewport.
	**/
	var scale(default,null) : Float;
	
}