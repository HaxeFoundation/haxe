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

/** <p>The HTML <em>ordered list</em> element (<code>&lt;ol&gt;</code>) represents an ordered list of items. Typically, ordered-list items are displayed with a preceding numbering, which can be of any form, like numerals, letters or Romans numerals or even simple bullets. This numbered style is not defined in the HTML description of the page, but in its associated CSS, using the <code><a rel="custom" href="https://developer.mozilla.org/en/CSS/list-style-type">list-style-type</a></code>
 property.</p>
<p>There is no limitation to the depth and imbrication of lists defined with the <code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/ol">&lt;ol&gt;</a></code>
 and <code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/ul">&lt;ul&gt;</a></code>
 elements.</p>
<div class="note"><strong>Usage note: </strong> The <code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/ol">&lt;ol&gt;</a></code>
 and <code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/ul">&lt;ul&gt;</a></code>
 both represent a list of items. They differ in the way that, with the <code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/ol">&lt;ol&gt;</a></code>
 element, the order is meaningful. As a rule of thumb to determine which one to use, try changing the order of the list items; if the meaning is changed, the <code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/ol">&lt;ol&gt;</a></code>
 element should be used, else the <code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/ul">&lt;ul&gt;</a></code>
 is adequate.</div><br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/HTML/Element/ol">MDN</a>. */
@:native("HTMLOListElement")
extern class OListElement extends Element
{
	/** This Boolean attribute hints that the list should be rendered in a compact style. The interpretation of this attribute depends on the user agent and it doesn't work in all browsers. <div class="note"><strong>Usage note:&nbsp;</strong>Do not use this attribute, as it has been deprecated: the <code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/ol">&lt;ol&gt;</a></code>
 element should be styled using <a title="en/CSS" rel="internal" href="https://developer.mozilla.org/en/CSS">CSS</a>. To give a similar effect than the <span>compact</span> attribute, the <a title="en/CSS" rel="internal" href="https://developer.mozilla.org/en/CSS">CSS</a> property <code><a rel="custom" href="https://developer.mozilla.org/en/CSS/line-height">line-height</a></code>
 can be used with a value of <span>80%</span>.</div> */
	var compact : Bool;

	/** This Boolean attribute specifies that the items of the item are specified in the reverse order, i.e. that the least important one is listed first. Browsers, by default, numbered the items in the reverse order too. */
	var reversed : Bool;

	/** This integer attribute specifies the start value for numbering the individual list items. Although the ordering type of list elements might be Roman numerals, such as XXXI, or letters, the value of start is always represented as a number. To start numbering elements from the letter "C", use <code>&lt;ol start="3"&gt;</code>. <div class="note"><strong>Note</strong>: that attribute was deprecated in HTML4, but reintroduced in HTML5.</div> */
	var start : Int;

	/** Indicates the numbering type: <ul> <li><span><code>'a'</code></span> indicates lowercase letters,</li> <li><span id="1284454877507S">&nbsp;</span><span><code>'<span id="1284454878023E">&nbsp;</span>A'</code></span> indicates uppercase letters,</li> <li><span><code>'i'</code></span> indicates lowercase Roman numerals,</li> <li><span><code>'I'</code></span> indicates uppercase Roman numerals,</li> <li>and <span><code>'1'</code></span> indicates numbers.</li> </ul> <p>The type set is used for the entire list unless a different 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/li#attr-type">type</a></code>
 attribute is used within an enclosed <code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/li">&lt;li&gt;</a></code>
 element.</p> <div class="note"><strong>Usage note:&nbsp;</strong>Do not use this attribute, as it has been deprecated: use the <a title="en/CSS" rel="internal" href="https://developer.mozilla.org/en/CSS">CSS</a> <code><a rel="custom" href="https://developer.mozilla.org/en/CSS/list-style-type">list-style-type</a></code>
 property instead.</div> */
	var type : String;

}
