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

/** The <em>HTML List item element</em> (<code>&lt;li&gt;</code>) is used to represent a list item. It should be contained in an ordered list (<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/ol">&lt;ol&gt;</a></code>
), an unordered list (<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/ul">&lt;ul&gt;</a></code>
) or a menu (<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/menu">&lt;menu&gt;</a></code>
), where it represents a single entity in that list. In menus and unordered lists, list items are ordinarily displayed using bullet points. In order lists, they are usually displayed with some ascending counter on the left such as a number or letter<br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/HTML/Element/li">MDN</a>. */
@:native("HTMLLIElement")
extern class LIElement extends Element
{
	/** This character attributes indicates the numbering type: <ul> <li><code>a</code>: lowercase letters</li> <li><code>A</code>: uppercase letters</li> <li><code>i</code>: lowercase Roman numerals</li> <li><code>I</code>: uppercase Roman numerals</li> <li><code>1</code>: numbers</li> </ul> This type overrides the one used by its parent <code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/ol">&lt;ol&gt;</a></code>
 element, if any.<br> <div class="note"><strong>Usage note:</strong> This attribute has been deprecated: use the CSS <code><a rel="custom" href="https://developer.mozilla.org/en/CSS/list-style-type">list-style-type</a></code>
 property instead.</div> */
	var type : String;

	/** This integer attributes indicates the current ordinal value of the item in the list as defined by the <code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/ol">&lt;ol&gt;</a></code>
 element. The only allowed value for this attribute is a number, even if the list is displayed with Roman numerals or letters. List items that follow this one continue numbering from the value set. The <strong>value</strong> attribute has no meaning for unordered lists (<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/ul">&lt;ul&gt;</a></code>
) or for menus (<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/menu">&lt;menu&gt;</a></code>
). <div class="note"><strong>Note</strong>: This attribute was deprecated in HTML4, but reintroduced in HTML5.</div> <div class="geckoVersionNote"> <p>
</p><div class="geckoVersionHeading">Gecko 9.0 note<div>(Firefox 9.0 / Thunderbird 9.0 / SeaMonkey 2.6)
</div></div>
<p></p> <p>Prior to <span title="(Firefox 9.0 / Thunderbird 9.0 / SeaMonkey 2.6)
">Gecko&nbsp;9.0</span>, negative values were incorrectly converted to 0. Starting in <span title="(Firefox 9.0 / Thunderbird 9.0 / SeaMonkey 2.6)
">Gecko&nbsp;9.0</span> all integer values are correctly parsed.</p> </div> */
	var value : Int;

}
