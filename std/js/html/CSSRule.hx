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

/** <p>An object implementing the <code>CSSRule</code> DOM interface represents a single CSS rule. References to a <code>CSSRule</code>-implementing object may be obtained by looking at a <a title="en/DOM/stylesheet" rel="internal" href="https://developer.mozilla.org/en/DOM/CSSStyleSheet">CSS style sheet's</a> <code><a title="en/DOM/CSSStyleSheet/cssRules" rel="internal" href="https://developer.mozilla.org/en/DOM/CSSStyleSheet">cssRules</a></code> list.</p>
<p>There are several kinds of rules. The <code>CSSRule</code> interface specifies the properties common to all rules, while properties unique to specific rule types are specified in the more specialized interfaces for those rules' respective types.</p><br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/cssRule">MDN</a>. */
@:native("CSSRule")
extern class CSSRule
{
	static inline var CHARSET_RULE : Int = 2;

	static inline var FONT_FACE_RULE : Int = 5;

	static inline var IMPORT_RULE : Int = 3;

	static inline var KEYFRAMES_RULE : Int = 7;

	static inline var KEYFRAME_RULE : Int = 8;

	static inline var MEDIA_RULE : Int = 4;

	static inline var PAGE_RULE : Int = 6;

	static inline var STYLE_RULE : Int = 1;

	static inline var UNKNOWN_RULE : Int = 0;

	/** Returns the textual representation of the rule, e.g. <code>"h1,h2 { font-size: 16pt }"</code> Setter throws DOMException. */
	var cssText : String;

	/** Returns the containing rule, otherwise <code>null</code>. E.g. if this rule is a style rule inside an <code><a title="en/CSS/@media" rel="internal" href="https://developer.mozilla.org/en/CSS/@media">@media</a></code> block, the parent rule would be that <code><a title="en/DOM/CSSMediaRule" rel="internal" href="https://developer.mozilla.org/en/DOM/CSSMediaRule">CSSMediaRule</a></code>. */
	var parentRule (default,null) : CSSRule;

	/** Returns the <code><a title="en/DOM/CSSStyleSheet" rel="internal" href="https://developer.mozilla.org/en/DOM/CSSStyleSheet">CSSStyleSheet</a></code> object for the style sheet that contains this rule */
	var parentStyleSheet (default,null) : CSSStyleSheet;

	/** One of the <a rel="custom" href="https://developer.mozilla.org/en/DOM/cssRule#Type_constants">Type constants</a>&nbsp;indicating the type of CSS&nbsp;rule. */
	var type (default,null) : Int;

}
