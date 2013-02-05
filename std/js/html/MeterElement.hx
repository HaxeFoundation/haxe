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
package js.html;

/** <p>The HTML <em>meter</em> element (<code>&lt;meter&gt;</code>) represents either a scalar value within a known range or a fractional value.</p>
<div class="note"><strong>Usage note: </strong>Unless the <strong>value</strong> attribute is between 0 and 1 (inclusive), the <strong>min</strong> attribute and <strong>max</strong> attribute should define the range so that the <strong>value</strong> attribute's value is within it.</div><br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/HTML/Element/meter">MDN</a>. */
@:native("HTMLMeterElement")
extern class MeterElement extends Element
{
	/** The lower numeric bound of the high end of the measured range. This must be less than the maximum value (<strong>max</strong> attribute), and it also must be greater than the low value and minimum value (<strong>low</strong> attribute and <strong>min</strong> attribute, respectively), if any are specified. If unspecified, or if greater than the maximum value, the <strong>high</strong> value is equal to the maximum value. Setter throws DOMException. */
	var high : Float;

	var labels(default,null) : NodeList;

	/** The upper numeric bound of the low end of the measured range. This must be greater than the minimum value (<strong>min</strong> attribute), and it also must be less than the high value and maximum value (<strong>high</strong> attribute and <strong>max</strong> attribute, respectively), if any are specified. If unspecified, or if less than the minimum value, the <strong>low</strong> value is equal to the minimum value. Setter throws DOMException. */
	var low : Float;

	/** The upper numeric bound of the measured range. This must be greater than the minimum value (<strong>min</strong> attribute), if specified. If unspecified, the maximum value is 1. Setter throws DOMException. */
	var max : Float;

	/** The lower numeric bound of the measured range. This must be less than the maximum value (<strong>max</strong> attribute), if specified. If unspecified, the minimum value is 0. Setter throws DOMException. */
	var min : Float;

	/** This attribute indicates the optimal numeric value. It must be within the range (as defined by the <strong>min</strong> attribute and <strong>max</strong> attribute). When used with the <strong>low</strong> attribute and <strong>high</strong> attribute, it gives an indication where along the range is considered preferable. For example, if it is between the <strong>min</strong> attribute and the <strong>low</strong> attribute, then the lower range is considered preferred. Setter throws DOMException. */
	var optimum : Float;

	/** The current numeric value. This must be between the minimum and maximum values (<strong>min</strong> attribute and <strong>max</strong> attribute) if they are specified. If unspecified or malformed, the value is 0. If specified, but not within the range given by the <strong>min</strong> attribute and <strong>max</strong> attribute, the value is equal to the nearest end of the range. Setter throws DOMException. */
	var value : Float;

}
