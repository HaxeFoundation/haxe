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

// This file is generated from mozilla\SVGSVGElement.webidl. Do not edit!

package js.html.svg;

/**
	The `SVGSVGElement` interface provides access to the properties of `svg` elements, as well as methods to manipulate them. This interface contains also various miscellaneous commonly-used utility methods, such as matrix operations and the ability to control the time of redraw on visual rendering devices.

	Documentation [SVGSVGElement](https://developer.mozilla.org/en-US/docs/Web/API/SVGSVGElement) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/SVGSVGElement$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/SVGSVGElement>
**/
@:native("SVGSVGElement")
extern class SVGElement extends GraphicsElement {
	static inline var SVG_ZOOMANDPAN_UNKNOWN : Int = 0;
	static inline var SVG_ZOOMANDPAN_DISABLE : Int = 1;
	static inline var SVG_ZOOMANDPAN_MAGNIFY : Int = 2;
	
	
	/**
		An `SVGAnimatedLength` corresponding to the `x` attribute of the given `svg` element.
	**/
	var x(default,null) : AnimatedLength;
	
	/**
		An `SVGAnimatedLength` corresponding to the `y` attribute of the given `svg` element.
	**/
	var y(default,null) : AnimatedLength;
	
	/**
		An `SVGAnimatedLength` corresponding to the `width` attribute of the given `svg` element.
	**/
	var width(default,null) : AnimatedLength;
	
	/**
		An `SVGAnimatedLength` corresponding to the `height` attribute of the given `svg` element.
	**/
	var height(default,null) : AnimatedLength;
	
	/**
		The initial view (i.e., before magnification and panning) of the current innermost SVG document fragment can be either the "standard" view, i.e., based on attributes on the `svg` element such as `viewBox`) or on a "custom" view (i.e., a hyperlink into a particular `view` or other element). If the initial view is the "standard" view, then this attribute is `false`. If the initial view is a "custom" view, then this attribute is `true`.
	**/
	var useCurrentView(default,null) : Bool;
	
	/**
		On an outermost `svg` element, this float attribute indicates the current scale factor relative to the initial view to take into account user magnification and panning operations. DOM attributes `currentScale` and `currentTranslate` are equivalent to the 2x3 matrix `[a b c d e f] = [currentScale 0 0 currentScale currentTranslate.x currentTranslate.y]`. If "magnification" is enabled (i.e., `zoomAndPan="magnify"`), then the effect is as if an extra transformation were placed at the outermost level on the SVG document fragment (i.e., outside the outermost `svg` element).
	**/
	var currentScale : Float;
	
	/**
		An `SVGPoint` representing the translation factor that takes into account user "magnification" corresponding to an outermost `svg` element. The behavior is undefined for svg elements that are not at the outermost level.
	**/
	var currentTranslate(default,null) : Point;
	var viewBox(default,null) : AnimatedRect;
	var preserveAspectRatio(default,null) : AnimatedPreserveAspectRatio;
	var zoomAndPan : Int;
	
	
	/**
		
		 Takes a time-out value which indicates that redraw shall not occur until:
		 the corresponding unsuspendRedraw() call has been made, an unsuspendRedrawAll() call has been made, or its timer has timed out.
		
		 In environments that do not support interactivity (e.g., print media), then redraw shall not be suspended. Calls to `suspendRedraw()` and `unsuspendRedraw()` should, but need not be, made in balanced pairs.
		
		 To suspend redraw actions as a collection of SVG DOM changes occur, precede the changes to the SVG DOM with a method call similar to:
		
		 suspendHandleID = suspendRedraw(maxWaitMilliseconds);
		
		 and follow the changes with a method call similar to:
		
		 unsuspendRedraw(suspendHandleID);
		
		 Note that multiple suspendRedraw calls can be used at once and that each such method call is treated independently of the other suspendRedraw method calls.
		 
	**/
	function suspendRedraw( maxWaitMilliseconds : Int ) : Int;
	
	/**
		Cancels a specified `suspendRedraw()` by providing a unique suspend handle ID that was returned by a previous `suspendRedraw()` call.
	**/
	function unsuspendRedraw( suspendHandleID : Int ) : Void;
	
	/**
		Cancels all currently active `suspendRedraw()` method calls. This method is most useful at the very end of a set of SVG DOM calls to ensure that all pending `suspendRedraw()` method calls have been cancelled.
	**/
	function unsuspendRedrawAll() : Void;
	
	/**
		In rendering environments supporting interactivity, forces the user agent to immediately redraw all regions of the viewport that require updating.
	**/
	function forceRedraw() : Void;
	
	/**
		Suspends (i.e., pauses) all currently running animations that are defined within the SVG document fragment corresponding to this `svg` element, causing the animation clock corresponding to this document fragment to stand still until it is unpaused.
	**/
	function pauseAnimations() : Void;
	
	/**
		Unsuspends (i.e., unpauses) currently running animations that are defined within the SVG document fragment, causing the animation clock to continue from the time at which it was suspended.
	**/
	function unpauseAnimations() : Void;
	
	/**
		Returns true if this SVG document fragment is in a paused state.
	**/
	function animationsPaused() : Bool;
	
	/**
		Returns the current time in seconds relative to the start time for the current SVG document fragment. If getCurrentTime is called before the document timeline has begun (for example, by script running in a `script` element before the document's SVGLoad event is dispatched), then 0 is returned.
	**/
	function getCurrentTime() : Float;
	
	/**
		Adjusts the clock for this SVG document fragment, establishing a new current time. If `setCurrentTime` is called before the document timeline has begun (for example, by script running in a `script` element before the document's SVGLoad event is dispatched), then the value of seconds in the last invocation of the method gives the time that the document will seek to once the document timeline has begun.
	**/
	function setCurrentTime( seconds : Float ) : Void;
	
	/**
		Unselects any selected objects, including any selections of text strings and type-in bars.
	**/
	function deselectAll() : Void;
	
	/**
		Creates an `SVGNumber` object outside of any document trees. The object is initialized to a value of zero.
	**/
	function createSVGNumber() : Number;
	
	/**
		Creates an `SVGLength` object outside of any document trees. The object is initialized to a value of zero user units.
	**/
	function createSVGLength() : Length;
	
	/**
		Creates an `SVGAngle` object outside of any document trees. The object is initialized to a value of zero degrees (unitless).
	**/
	function createSVGAngle() : Angle;
	
	/**
		Creates an `SVGPoint` object outside of any document trees. The object is initialized to the point (0,0) in the user coordinate system.
	**/
	function createSVGPoint() : Point;
	
	/**
		Creates an `SVGMatrix` object outside of any document trees. The object is initialized to the identity matrix.
	**/
	function createSVGMatrix() : Matrix;
	
	/**
		Creates an `SVGRect` object outside of any document trees. The object is initialized such that all values are set to 0 user units.
	**/
	function createSVGRect() : Rect;
	
	/**
		Creates an `SVGTransform` object outside of any document trees. The object is initialized to an identity matrix transform (`SVG_TRANSFORM_MATRIX`).
	**/
	function createSVGTransform() : Transform;
	
	/**
		Creates an `SVGTransform` object outside of any document trees. The object is initialized to the given matrix transform (i.e., `SVG_TRANSFORM_MATRIX`). The values from the parameter matrix are copied, the matrix parameter is not adopted as `SVGTransform::matrix`.
	**/
	function createSVGTransformFromMatrix( matrix : Matrix ) : Transform;
	
	/**
		Searches this SVG document fragment (i.e., the search is restricted to a subset of the document tree) for an Element whose id is given by elementId. If an Element is found, that Element is returned. If no such element exists, returns null. Behavior is not defined if more than one element has this id.
	**/
	function getElementById( elementId : String ) : js.html.Element;
}