/*
 * Copyright (C)2005-2012 Haxe Foundation
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

/** DOM table column objects (which may correspond to <code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/col">&lt;col&gt;</a></code>
&nbsp;or <code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/colgroup">&lt;colgroup&gt;</a></code>
 HTML elements) expose the <a target="_blank" href="http://www.w3.org/TR/html5/tabular-data.html#htmltablecolelement" rel="external nofollow" class=" external" title="http://www.w3.org/TR/html5/tabular-data.html#htmltablecolelement">HTMLTableColElement</a> (or <span><a href="https://developer.mozilla.org/en/HTML" rel="custom nofollow">HTML 4</a></span> <a target="_blank" title="http://www.w3.org/TR/DOM-Level-2-HTML/html.html#ID-84150186" href="http://www.w3.org/TR/DOM-Level-2-HTML/html.html#ID-84150186" rel="external nofollow" class=" external"><code>HTMLTableColElement</code></a>) interface, which provides special properties (beyond the regular <a href="https://developer.mozilla.org/en/DOM/element" rel="internal">element</a> object interface they also have available to them by inheritance) for manipulating table column elements.<br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/HTMLTableColElement">MDN</a>. */
@:native("HTMLTableColElement")
extern class TableColElement extends Element
{
    /** Indicates the horizontal alignment of the cell data in the column. */
    var align :String;

    /** Alignment character for cell data. */
    var ch :String;

    /** Offset for the alignment character. */
    var chOff :String;

    /** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/col#attr-span">span</a></code>
 HTML&nbsp;attribute, indicating the number of columns to apply this object's attributes to. Must be a positive integer. */
    var span :Int;

    /** Indicates the vertical alignment of the cell data in the column. */
    var vAlign :String;

    /** Default column width. */
    var width :String;

}
