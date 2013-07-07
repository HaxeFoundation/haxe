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

/** <p>The HTML <em>unordered list</em> element (<code>&lt;ul&gt;</code>) represents an unordered list of items, namely a collection of items that do not have a numerical ordering, and their order in the list is meaningless. Typically, unordered-list items are displayed with a bullet, which can be of several forms, like a dot, a circle or a squared. The bullet style is not defined in the HTML description of the page, but in its associated CSS, using the <code><a rel="custom" href="https://developer.mozilla.org/en/CSS/list-style-type">list-style-type</a></code>
 property.</p>
<p>There is no limitation to the depth and imbrication of lists defined with the <code><a rel="custom" href="/api/js/html/OListElement">&lt;ol&gt;</a></code>
 and <code><a rel="custom" href="/api/js/html/UListElement">&lt;ul&gt;</a></code>
 elements.</p>
<div class="note"><strong>Usage note: </strong> The <code><a rel="custom" href="/api/js/html/OListElement">&lt;ol&gt;</a></code>
 and <code><a rel="custom" href="/api/js/html/UListElement">&lt;ul&gt;</a></code>
 both represent a list of items. They differ in the way that, with the <code><a rel="custom" href="/api/js/html/OListElement">&lt;ol&gt;</a></code>
 element, the order is meaningful. As a rule of thumb to determine which one to use, try changing the order of the list items; if the meaning is changed, the <code><a rel="custom" href="/api/js/html/OListElement">&lt;ol&gt;</a></code>
 element should be used, else the <code><a rel="custom" href="/api/js/html/UListElement">&lt;ul&gt;</a></code>
 is adequate.</div><br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/HTML/Element/ul">MDN</a>. */
@:native("HTMLUListElement")
extern class UListElement extends Element
{
	/** This Boolean attribute hints that the list should be rendered in a compact style. The interpretation of this attribute depends on the user agent and it doesn't work in all browsers. <div class="note"><strong>Usage note:&nbsp;</strong>Do not use this attribute, as it has been deprecated: the <code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/ol">&lt;ol&gt;</a></code>
 element should be styled using <a title="en/CSS" rel="internal" href="https://developer.mozilla.org/en/CSS">CSS</a>. To give a similar effect than the <span>compact</span> attribute, the <a title="en/CSS" rel="internal" href="https://developer.mozilla.org/en/CSS">CSS</a> property <a title="en/CSS/line-height" rel="internal" href="https://developer.mozilla.org/en/CSS/line-height">line-height</a> can be used with a value of <span>80%</span>.</div> */
	var compact : Bool;

	/** Used to set the bullet style for the list. The values defined under <a title="en/HTML3.2" rel="internal" href="https://developer.mozilla.org/en/HTML3.2" class="new ">HTML3.2</a> and the transitional version of <a title="en/HTML4.01" rel="internal" href="https://developer.mozilla.org/en/HTML4.01" class="new ">HTML 4.0/4.01</a> are<span>:</span> <ul> <li><code>circle</code>,</li> <li><code>disc</code>,</li> <li>and <code>square</code>.</li> </ul> <p>A fourth bullet type has been defined in the WebTV interface, but not all browsers support it: <code>triangle.</code></p> <p>If not present and if no <a title="en/CSS" rel="internal" href="https://developer.mozilla.org/en/CSS">CSS</a> <code><a rel="custom" href="https://developer.mozilla.org/en/CSS/list-style-type">list-style-type</a></code>
 property does apply to the element, the user agent decide to use a kind of bullets depending on the nesting level of the list.</p> <div class="note"><strong>Usage note:</strong> Do not use this attribute, as it has been deprecated: use the <a title="en/CSS" rel="internal" href="https://developer.mozilla.org/en/CSS">CSS</a> <code><a rel="custom" href="https://developer.mozilla.org/en/CSS/list-style-type">list-style-type</a></code>
 property instead.</div> */
	var type : String;

}
