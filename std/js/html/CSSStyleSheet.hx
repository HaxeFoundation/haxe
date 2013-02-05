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

/** <p>An object implementing the <code>CSSStyleSheet</code> interface represents a single <a title="en/CSS" rel="internal" href="https://developer.mozilla.org/en/CSS">CSS</a> style sheet.</p>
<p>A CSS style sheet consists of CSS rules, each of which can be manipulated through an object that corresponds to that rule and that implements the <code><a title="en/DOM/cssRule" rel="internal" href="https://developer.mozilla.org/en/DOM/cssRule">CSSRule</a></code> interface. The <code>CSSStyleSheet</code> itself lets you examine and modify its corresponding style sheet, including its list of rules.</p>
<p>In practice, every <code>CSSStyleSheet</code> also implements the more generic <code><a title="en/DOM/StyleSheet" rel="internal" href="https://developer.mozilla.org/en/DOM/stylesheet">StyleSheet</a></code> interface. A list of <code>CSSStyleSheet</code>-implementing objects corresponding to the style sheets for a given document can be reached by the <code><a title="en/DOM/document.styleSheets" rel="internal" href="https://developer.mozilla.org/en/DOM/document.styleSheets">document.styleSheets</a></code> property, if the document is styled by an external CSS style sheet or an inline <code><a title="en/HTML/element/style" rel="internal" href="https://developer.mozilla.org/en/HTML/Element/style">style</a></code> element.</p><br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/CSSStyleSheet">MDN</a>. */
@:native("CSSStyleSheet")
extern class CSSStyleSheet extends StyleSheet
{
	/** Returns a <code><a title="en/DOM/CSSRuleList" rel="internal" href="https://developer.mozilla.org/en/DOM/CSSRuleList">CSSRuleList</a></code> of the CSS rules in the style sheet. */
	var cssRules(default,null) : CSSRuleList;

	/** If this style sheet is imported into the document using an <code><a title="en/CSS/@import" rel="internal" href="https://developer.mozilla.org/en/CSS/@import">@import</a></code> rule, the <code>ownerRule</code> property will return that <code><a title="en/DOM/CSSImportRule" rel="internal" href="https://developer.mozilla.org/en/DOM/CSSImportRule" class="new ">CSSImportRule</a></code>, otherwise it returns <code>null</code>. */
	var ownerRule(default,null) : CSSRule;

	var rules(default,null) : CSSRuleList;

	function addRule( selector : String, style : String, ?index : Int ) : Int;

	function deleteRule( index : Int ) : Void;

	function insertRule( rule : String, index : Int ) : Int;

	function removeRule( index : Int ) : Void;

}
