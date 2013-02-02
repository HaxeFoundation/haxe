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

/** The <code>marker</code> element defines the graphics that is to be used for drawing arrowheads or polymarkers on a given <code><a rel="custom" href="https://developer.mozilla.org/en/SVG/Element/path">&lt;path&gt;</a></code>
, <code><a rel="custom" href="https://developer.mozilla.org/en/SVG/Element/line">&lt;line&gt;</a></code>
, <code><a rel="custom" href="https://developer.mozilla.org/en/SVG/Element/polyline">&lt;polyline&gt;</a></code>
 or <code><a rel="custom" href="https://developer.mozilla.org/en/SVG/Element/polygon">&lt;polygon&gt;</a></code>
 element.<br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/SVG/Element/marker">MDN</a>. */
@:native("SVGMarkerElement")
extern class MarkerElement extends Element
{
	static inline var SVG_MARKERUNITS_STROKEWIDTH : Int = 2;

	static inline var SVG_MARKERUNITS_UNKNOWN : Int = 0;

	static inline var SVG_MARKERUNITS_USERSPACEONUSE : Int = 1;

	static inline var SVG_MARKER_ORIENT_ANGLE : Int = 2;

	static inline var SVG_MARKER_ORIENT_AUTO : Int = 1;

	static inline var SVG_MARKER_ORIENT_UNKNOWN : Int = 0;

	var markerHeight (default,null) : AnimatedLength;

	var markerUnits (default,null) : AnimatedEnumeration;

	var markerWidth (default,null) : AnimatedLength;

	var orientAngle (default,null) : AnimatedAngle;

	var orientType (default,null) : AnimatedEnumeration;

	var refX (default,null) : AnimatedLength;

	var refY (default,null) : AnimatedLength;

	function setOrientToAngle( angle : Angle ) : Void;

	function setOrientToAuto() : Void;

}
