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

/** An object implementing the <code>StyleSheet</code> interface represents a single style sheet.&nbsp; CSS style sheets will further implement the more specialized <code><a title="en/DOM/CSSStyleSheet" rel="internal" href="https://developer.mozilla.org/en/DOM/CSSStyleSheet">CSSStyleSheet</a></code> interface.<br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/stylesheet">MDN</a>. */
@:native("StyleSheet")
extern class StyleSheet
{
	/** This property indicates whether the current stylesheet has been applied or not. */
	var disabled : Bool;

	/** Returns the location of the stylesheet. */
	var href (default,null) : String;

	/** Specifies the intended destination medium for style information. */
	var media (default,null) : MediaList;

	/** Returns the node that associates this style sheet with the document. */
	var ownerNode (default,null) : Node;

	/** Returns the stylesheet that is including this one, if any. */
	var parentStyleSheet (default,null) : StyleSheet;

	/** Returns the advisory title of the current style sheet. */
	var title (default,null) : String;

	/** Specifies the style sheet language for this style sheet. */
	var type (default,null) : String;

}
