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

/** DOM Datalist objects expose the <a class=" external" title="http://www.whatwg.org/specs/web-apps/current-work/multipage/the-button-element.html#the-datalist-element" rel="external" href="http://www.whatwg.org/specs/web-apps/current-work/multipage/the-button-element.html#the-datalist-element" target="_blank">HTMLDataListElement</a> interface, which provides special properties (beyond the <a href="https://developer.mozilla.org/en/DOM/element" rel="internal">element</a> object interface they also have available to them by inheritance) to manipulate <code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/datalist">&lt;datalist&gt;</a></code>
 elements and their content.<br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/HTMLDataListElement">MDN</a>. */
@:native("HTMLDataListElement")
extern class DataListElement extends Element
{
	/** A collection of the contained option elements. */
	var options (default,null) : HTMLCollection;

}
