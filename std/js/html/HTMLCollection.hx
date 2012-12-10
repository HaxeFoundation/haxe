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

/** <p><code>HTMLCollection</code> is an interface representing a generic collection of elements (in document order) and offers methods and properties for traversing the list.</p>
<div class="note"><strong>Note:</strong> This interface is called <code>HTMLCollection</code> for historical reasons (before DOM4, collections implementing this interface could only have HTML elements as their items).</div>
<p><code>HTMLCollection</code>s in the HTML DOM are live; they are automatically updated when the underlying document is changed.</p><br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/HTMLCollection">MDN</a>. */
@:native("HTMLCollection")
extern class HTMLCollection implements ArrayAccess<Node>
{
    /** The number of items in the collection. <strong>Read only</strong>. */
    var length (default,null) :Int;

    function item (index :Int) :Node;

    function namedItem (name :String) :Node;

}
