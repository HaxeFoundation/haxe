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

/** DOM <code>hr</code> elements expose the <a target="_blank" rel="external nofollow" class=" external" title="http://www.w3.org/TR/html5/grouping-content.html#htmlhrelement" href="http://www.w3.org/TR/html5/grouping-content.html#htmlhrelement">HTMLHRElement</a> (or <span><a rel="custom nofollow" href="https://developer.mozilla.org/en/HTML">HTML 4</a></span> <a target="_blank" title="http://www.w3.org/TR/DOM-Level-2-HTML/html.html#ID-68228811" rel="external nofollow" class=" external" href="http://www.w3.org/TR/DOM-Level-2-HTML/html.html#ID-68228811"><code>HTMLHRElement</code></a>) interface, which provides special properties (beyond the regular <a rel="internal" href="/api/js/html/Element">element</a> object interface they also have available to them by inheritance) for manipulating <code>hr</code> elements. In <span><a rel="custom nofollow" href="https://developer.mozilla.org/en/HTML/HTML5">HTML 5</a></span>, this interface inherits from HTMLElement, but defines no additional members.<br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/HTMLHRElement">MDN</a>. */
@:native("HTMLHRElement")
extern class HRElement extends Element
{
	/** Enumerated attribute indicating alignment of the rule with respect to the surrounding context. */
	var align : String;

	var noShade : Bool;

	/** The height of the rule. */
	var size : String;

	/** The width of the rule on the page. */
	var width : String;

}
