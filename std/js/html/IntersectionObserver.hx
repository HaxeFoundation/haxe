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
	provides a way to asynchronously observe changes in the intersection of a target element with an ancestor element or with a top-level document's viewport.

	Documentation [IntersectionObserver](https://developer.mozilla.org/en-US/docs/Web/API/IntersectionObserver) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/IntersectionObserver$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/IntersectionObserver>
**/
@:native("IntersectionObserver")
extern class IntersectionObserver {
	
	/**
		A specific ancestor of the target `element` being observed. If no value was passed to the constructor or this is `null`, the top-level document's viewport is used.
	**/
	var root(default,null) : Element;
	
	/**
		An offset rectangle applied to the root's `bounding box` when calculating intersections, effectively shrinking or growing the root for calculation purposes. The value returned by this property may not be the same as the one specified when calling the constructor as it may be changed to match internal requirements. Each offset can be expressed in pixels (`px`) or as a percentage (`%`). The default is "0px 0px 0px 0px".
	**/
	var rootMargin(default,null) : String;
	
	/**
		A list of thresholds, sorted in increasing numeric order, where each threshold is a ratio of intersection area to bounding box area of an observed target. Notifications for a target are generated when any of the thresholds are crossed for that target. If no value was passed to the constructor, 0 is used.
	**/
	var thresholds(default,null) : Array<Float>;
	
	/** @throws DOMError */
	function new( intersectionCallback : Array<IntersectionObserverEntry> -> IntersectionObserver -> Void, ?options : IntersectionObserverInit ) : Void;
	
	/**
		Tells the `IntersectionObserver` a target element to observe.
	**/
	function observe( target : Element ) : Void;
	
	/**
		Tells the `IntersectionObserver` to stop observing a particular target element.
	**/
	function unobserve( target : Element ) : Void;
	
	/**
		Stops the `IntersectionObserver` object from observing any target.
	**/
	function disconnect() : Void;
	
	/**
		Returns an array of `IntersectionObserverEntry` objects for all observed targets.
	**/
	function takeRecords() : Array<IntersectionObserverEntry>;
}