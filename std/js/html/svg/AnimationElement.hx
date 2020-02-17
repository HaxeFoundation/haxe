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

// This file is generated from mozilla\SVGAnimationElement.webidl. Do not edit!

package js.html.svg;

/**
	The `SVGAnimationElement` interface is the base interface for all of the animation element interfaces: `SVGAnimateElement`, `SVGSetElement`, `SVGAnimateColorElement`, `SVGAnimateMotionElement` and `SVGAnimateTransformElement`.

	Documentation [SVGAnimationElement](https://developer.mozilla.org/en-US/docs/Web/API/SVGAnimationElement) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/SVGAnimationElement$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/SVGAnimationElement>
**/
@:native("SVGAnimationElement")
extern class AnimationElement extends Element {
	
	/**
		An `SVGElement` representing the element which is being animated. If no target element is being animated (for example, because the `href` specifies an unknown element) the value returned is `null`.
	**/
	var targetElement(default,null) : Element;
	var requiredFeatures(default,null) : StringList;
	var requiredExtensions(default,null) : StringList;
	var systemLanguage(default,null) : StringList;
	
	
	/**
		Returns a float representing the begin time, in seconds, for this animation element's current interval, if it exists, regardless of whether the interval has begun yet. If there is no current interval, then a `DOMException` with code `INVALID_STATE_ERR` is thrown.
		@throws DOMError
	**/
	function getStartTime() : Float;
	
	/**
		Returns a float representing the current time in seconds relative to time zero for the given time container.
	**/
	function getCurrentTime() : Float;
	
	/**
		Returns a float representing the number of seconds for the simple duration for this animation. If the simple duration is undefined (e.g., the end time is indefinite), then a `DOMException` with code `NOT_SUPPORTED_ERR` is raised.
		@throws DOMError
	**/
	function getSimpleDuration() : Float;
	
	/**
		Creates a begin instance time for the current time. The new instance time is added to the begin instance times list. The behavior of this method is equivalent to `beginElementAt(0)`.
		@throws DOMError
	**/
	function beginElement() : Void;
	
	/**
		
		 Creates a begin instance time for the current time plus the specified offset. The new instance time is added to the begin instance times list.
		 
		@throws DOMError
	**/
	function beginElementAt( offset : Float ) : Void;
	
	/**
		
		 Creates an end instance time for the current time. The new instance time is added to the end instance times list. The behavior of this method is equivalent to `endElementAt(0)`.
		 
		@throws DOMError
	**/
	function endElement() : Void;
	
	/**
		
		 Creates a end instance time for the current time plus the specified offset. The new instance time is added to the end instance times list.
		 
		@throws DOMError
	**/
	function endElementAt( offset : Float ) : Void;
	function hasExtension( extension : String ) : Bool;
}