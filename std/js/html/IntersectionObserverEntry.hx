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

// This file is generated from mozilla\IntersectionObserver.webidl. Do not edit!

package js.html;

/**
	The `IntersectionObserverEntry` interface of the Intersection Observer API describes the intersection between the target element and its root container at a specific moment of transition.

	Documentation [IntersectionObserverEntry](https://developer.mozilla.org/en-US/docs/Web/API/IntersectionObserverEntry) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/IntersectionObserverEntry$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/IntersectionObserverEntry>
**/
@:native("IntersectionObserverEntry")
extern class IntersectionObserverEntry {
	
	/**
		A `DOMHighResTimeStamp` indicating the time at which the intersection was recorded, relative to the `IntersectionObserver`'s time origin.
	**/
	var time(default,null) : Float;
	
	/**
		Returns a `DOMRectReadOnly` for the intersection observer's root.
	**/
	var rootBounds(default,null) : DOMRectReadOnly;
	
	/**
		Returns the bounds rectangle of the target element as a `DOMRectReadOnly`. The bounds are computed as described in the documentation for `Element.getBoundingClientRect()`.
	**/
	var boundingClientRect(default,null) : DOMRectReadOnly;
	
	/**
		Returns a `DOMRectReadOnly` representing the target's visible area.
	**/
	var intersectionRect(default,null) : DOMRectReadOnly;
	
	/**
		A Boolean value which is `true` if the target element intersects with the intersection observer's root. If this is `true`, then, the `IntersectionObserverEntry` describes a transition into a state of intersection; if it's `false`, then you know the transition is from intersecting to not-intersecting.
	**/
	var isIntersecting(default,null) : Bool;
	
	/**
		Returns the ratio of the `intersectionRect` to the `boundingClientRect`.
	**/
	var intersectionRatio(default,null) : Float;
	
	/**
		The `Element` whose intersection with the root changed.
	**/
	var target(default,null) : Element;
	
}