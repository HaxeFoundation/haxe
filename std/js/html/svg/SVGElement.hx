/*
 * Copyright (C)2005-2013 Haxe Foundation
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

// This file is generated, do not edit!
package js.html.svg;

/** The <code>SVGSVGElement</code> interface provides access to the properties of <code><a rel="custom" href="https://developer.mozilla.org/en/SVG/Element/svg">&lt;svg&gt;</a></code>
 elements, as well as methods to manipulate them. This interface contains also various miscellaneous commonly-used utility methods, such as matrix operations and the ability to control the time of redraw on visual rendering devices.<br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/SVGSVGElement">MDN</a>. */
@:native("SVGSVGElement")
extern class SVGElement extends Element
{
	/** Corresponds to attribute 
<code><a rel="custom" href="https://developer.mozilla.org/en/SVG/Attribute/contentScriptType">contentScriptType</a></code> on the given <code><a rel="custom" href="https://developer.mozilla.org/en/SVG/Element/svg">&lt;svg&gt;</a></code>
 element. */
	var contentScriptType : String;

	/** Corresponds to attribute 
<code><a rel="custom" href="https://developer.mozilla.org/en/SVG/Attribute/contentStyleType">contentStyleType</a></code> on the given <code><a rel="custom" href="https://developer.mozilla.org/en/SVG/Element/svg">&lt;svg&gt;</a></code>
 element. */
	var contentStyleType : String;

	/** On an outermost <code><a rel="custom" href="https://developer.mozilla.org/en/SVG/Element/svg">&lt;svg&gt;</a></code>
 element, this attribute indicates the current scale factor relative to the initial view to take into account user magnification and panning operations. DOM attributes <code>currentScale</code> and <code>currentTranslate</code> are equivalent to the 2x3 matrix <code>[a b c d e f] = [currentScale 0 0 currentScale currentTranslate.x currentTranslate.y]</code>. If "magnification" is enabled (i.e., <code>zoomAndPan="magnify"</code>), then the effect is as if an extra transformation were placed at the outermost level on the SVG document fragment (i.e., outside the outermost <code><a rel="custom" href="https://developer.mozilla.org/en/SVG/Element/svg">&lt;svg&gt;</a></code>
 element). */
	var currentScale : Float;

	/** On an outermost <code><a rel="custom" href="https://developer.mozilla.org/en/SVG/Element/svg">&lt;svg&gt;</a></code>
 element, the corresponding translation factor that takes into account user "magnification". */
	var currentTranslate(default,null) : Point;

	/** The definition of the initial view (i.e., before magnification and panning) of the current innermost SVG document fragment. The meaning depends on the situation:<br> <ul> <li>If the initial view was a "standard" view, then: <ul> <li>the values for 
<code><a rel="internal" href="https://developer.mozilla.org/en/SVG/Attribute/viewBox" class="new">viewBox</a></code>, 
<code><a rel="custom" href="https://developer.mozilla.org/en/SVG/Attribute/preserveAspectRatio">preserveAspectRatio</a></code> and 
<code><a rel="internal" href="https://developer.mozilla.org/en/SVG/Attribute/zoomAndPan" class="new">zoomAndPan</a></code> within 
<code><a rel="internal" href="https://developer.mozilla.org/en/SVG/Attribute/currentView" class="new">currentView</a></code> will match the values for the corresponding DOM attributes that are on <code>SVGSVGElement</code> directly</li> <li>the values for 
<code><a rel="custom" href="https://developer.mozilla.org/en/SVG/Attribute/transform">transform</a></code> and 
<code><a rel="internal" href="https://developer.mozilla.org/en/SVG/Attribute/viewTarget" class="new">viewTarget</a></code> within 
<code><a rel="internal" href="https://developer.mozilla.org/en/SVG/Attribute/currentView" class="new">currentView</a></code> will be null</li> </ul> </li> <li>If the initial view was a link into a <code><a rel="custom" href="https://developer.mozilla.org/en/SVG/Element/view">&lt;view&gt;</a></code>
 element, then: <ul> <li>the values for 
<code><a rel="internal" href="https://developer.mozilla.org/en/SVG/Attribute/viewBox" class="new">viewBox</a></code>, 
<code><a rel="custom" href="https://developer.mozilla.org/en/SVG/Attribute/preserveAspectRatio">preserveAspectRatio</a></code> and 
<code><a rel="internal" href="https://developer.mozilla.org/en/SVG/Attribute/zoomAndPan" class="new">zoomAndPan</a></code> within 
<code><a rel="internal" href="https://developer.mozilla.org/en/SVG/Attribute/currentView" class="new">currentView</a></code> will correspond to the corresponding attributes for the given <code><a rel="custom" href="https://developer.mozilla.org/en/SVG/Element/view">&lt;view&gt;</a></code>
 element</li> <li>the values for 
<code><a rel="custom" href="https://developer.mozilla.org/en/SVG/Attribute/transform">transform</a></code> and 
<code><a rel="internal" href="https://developer.mozilla.org/en/SVG/Attribute/viewTarget" class="new">viewTarget</a></code> within 
<code><a rel="internal" href="https://developer.mozilla.org/en/SVG/Attribute/currentView" class="new">currentView</a></code> will be null</li> </ul> </li> <li>If the initial view was a link into another element (i.e., other than a <code><a rel="custom" href="https://developer.mozilla.org/en/SVG/Element/view">&lt;view&gt;</a></code>
), then: <ul> <li>the values for 
<code><a rel="internal" href="https://developer.mozilla.org/en/SVG/Attribute/viewBox" class="new">viewBox</a></code>, 
<code><a rel="custom" href="https://developer.mozilla.org/en/SVG/Attribute/preserveAspectRatio">preserveAspectRatio</a></code> and 
<code><a rel="internal" href="https://developer.mozilla.org/en/SVG/Attribute/zoomAndPan" class="new">zoomAndPan</a></code> within 
<code><a rel="internal" href="https://developer.mozilla.org/en/SVG/Attribute/currentView" class="new">currentView</a></code> will match the values for the corresponding DOM attributes that are on <code>SVGSVGElement</code> directly for the closest ancestor <code><a rel="custom" href="https://developer.mozilla.org/en/SVG/Element/svg">&lt;svg&gt;</a></code>
 element</li> <li>the values for 
<code><a rel="custom" href="https://developer.mozilla.org/en/SVG/Attribute/transform">transform</a></code> within 
<code><a rel="internal" href="https://developer.mozilla.org/en/SVG/Attribute/currentView" class="new">currentView</a></code> will be null</li> <li>the 
<code><a rel="internal" href="https://developer.mozilla.org/en/SVG/Attribute/viewTarget" class="new">viewTarget</a></code> within 
<code><a rel="internal" href="https://developer.mozilla.org/en/SVG/Attribute/currentView" class="new">currentView</a></code> will represent the target of the link</li> </ul> </li> <li>If the initial view was a link into the SVG document fragment using an SVG view specification fragment identifier (i.e., #svgView(...)), then: <ul> <li>the values for 
<code><a rel="internal" href="https://developer.mozilla.org/en/SVG/Attribute/viewBox" class="new">viewBox</a></code>, 
<code><a rel="custom" href="https://developer.mozilla.org/en/SVG/Attribute/preserveAspectRatio">preserveAspectRatio</a></code>, 
<code><a rel="internal" href="https://developer.mozilla.org/en/SVG/Attribute/zoomAndPan" class="new">zoomAndPan</a></code>, 
<code><a rel="custom" href="https://developer.mozilla.org/en/SVG/Attribute/transform">transform</a></code> and 
<code><a rel="internal" href="https://developer.mozilla.org/en/SVG/Attribute/viewTarget" class="new">viewTarget</a></code> within 
<code><a rel="internal" href="https://developer.mozilla.org/en/SVG/Attribute/currentView" class="new">currentView</a></code> will correspond to the values from the SVG view specification fragment identifier</li> </ul> </li> </ul> */
	var currentView(default,null) : ViewSpec;

	/** Corresponds to attribute 
<code><a rel="custom" href="https://developer.mozilla.org/en/SVG/Attribute/height">height</a></code> on the given <code><a rel="custom" href="https://developer.mozilla.org/en/SVG/Element/svg">&lt;svg&gt;</a></code>
 element. */
	var height(default,null) : AnimatedLength;

	/** Size of a pixel units (as defined by CSS2) along the x-axis of the viewport, which represents a unit somewhere in the range of 70dpi to 120dpi, and, on systems that support this, might actually match the characteristics of the target medium. On systems where it is impossible to know the size of a pixel, a suitable default pixel size is provided. */
	var pixelUnitToMillimeterX(default,null) : Float;

	/** Corresponding size of a pixel unit along the y-axis of the viewport. */
	var pixelUnitToMillimeterY(default,null) : Float;

	/** User interface (UI) events in DOM Level 2 indicate the screen positions at which the given UI event occurred. When the browser actually knows the physical size of a "screen unit", this attribute will express that information; otherwise, user agents will provide a suitable default value such as .28mm. */
	var screenPixelToMillimeterX(default,null) : Float;

	/** Corresponding size of a screen pixel along the y-axis of the viewport. */
	var screenPixelToMillimeterY(default,null) : Float;

	/** The initial view (i.e., before magnification and panning) of the current innermost SVG document fragment can be either the "standard" view (i.e., based on attributes on the <code><a rel="custom" href="https://developer.mozilla.org/en/SVG/Element/svg">&lt;svg&gt;</a></code>
 element such as 
<code><a rel="internal" href="https://developer.mozilla.org/en/SVG/Attribute/viewBox" class="new">viewBox</a></code>) or to a "custom" view (i.e., a hyperlink into a particular <code><a rel="custom" href="https://developer.mozilla.org/en/SVG/Element/view">&lt;view&gt;</a></code>
 or other element). If the initial view is the "standard" view, then this attribute is false. If the initial view is a "custom" view, then this attribute is true. */
	var useCurrentView(default,null) : Bool;

	/** The position and size of the viewport (implicit or explicit) that corresponds to this <code><a rel="custom" href="https://developer.mozilla.org/en/SVG/Element/svg">&lt;svg&gt;</a></code>
 element. When the browser is actually rendering the content, then the position and size values represent the actual values when rendering. The position and size values are unitless values in the coordinate system of the parent element. If no parent element exists (i.e., <code><a rel="custom" href="https://developer.mozilla.org/en/SVG/Element/svg">&lt;svg&gt;</a></code>
 element represents the root of the document tree), if this SVG document is embedded as part of another document (e.g., via the HTML <code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/object">&lt;object&gt;</a></code>
 element), then the position and size are unitless values in the coordinate system of the parent document. (If the parent uses CSS or XSL layout, then unitless values represent pixel units for the current CSS or XSL viewport.) */
	var viewport(default,null) : Rect;

	/** Corresponds to attribute 
<code><a rel="custom" href="https://developer.mozilla.org/en/SVG/Attribute/width">width</a></code> on the given <code><a rel="custom" href="https://developer.mozilla.org/en/SVG/Element/svg">&lt;svg&gt;</a></code>
 element. */
	var width(default,null) : AnimatedLength;

	var x(default,null) : AnimatedLength;

	var y(default,null) : AnimatedLength;

	function animationsPaused() : Bool;

	function checkEnclosure( element : Element, rect : Rect ) : Bool;

	function checkIntersection( element : Element, rect : Rect ) : Bool;

	function createSVGAngle() : Angle;

	function createSVGLength() : Length;

	function createSVGMatrix() : Matrix;

	function createSVGNumber() : Number;

	function createSVGPoint() : Point;

	function createSVGRect() : Rect;

	function createSVGTransform() : Transform;

	function createSVGTransformFromMatrix( matrix : Matrix ) : Transform;

	function deselectAll() : Void;

	function forceRedraw() : Void;

	function getCurrentTime() : Float;

	function getElementById( elementId : String ) : js.html.Element;

	function getEnclosureList( rect : Rect, referenceElement : Element ) : js.html.NodeList;

	function getIntersectionList( rect : Rect, referenceElement : Element ) : js.html.NodeList;

	function pauseAnimations() : Void;

	function setCurrentTime( seconds : Float ) : Void;

	function suspendRedraw( maxWaitMilliseconds : Int ) : Int;

	function unpauseAnimations() : Void;

	function unsuspendRedraw( suspendHandleId : Int ) : Void;

	function unsuspendRedrawAll() : Void;

}
