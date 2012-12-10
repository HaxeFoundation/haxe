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

/** <p>DocumentFragment has no properties or methods of its own, but inherits from <a title="En/DOM/Node" class="internal" rel="internal" href="https://developer.mozilla.org/En/DOM/Node"><code>Node</code></a>. </p>
<p>A <code><a class="external" rel="external" href="http://www.w3.org/TR/DOM-Level-2-Core/core.html#ID-B63ED1A3" title="http://www.w3.org/TR/DOM-Level-2-Core/core.html#ID-B63ED1A3" target="_blank">DocumentFragment</a></code> is a minimal document object that has no parent. It is used as a light-weight version of document to store well-formed or potentially non-well-formed fragments of XML.</p>
<p>See <a title="En/DOM/Node" class="internal" rel="internal" href="https://developer.mozilla.org/En/DOM/Node"><code>Node</code></a> for a listing of its properties, constants and methods.</p>
<p>Various other methods can take a document fragment as an argument (e.g., any <code><a class="external" rel="external" href="http://www.w3.org/TR/DOM-Level-2-Core/core.html#ID-1950641247" title="http://www.w3.org/TR/DOM-Level-2-Core/core.html#ID-1950641247" target="_blank">Node</a></code> interface methods such as <code><a title="En/DOM/Node.appendChild" rel="internal" href="https://developer.mozilla.org/En/DOM/Node.appendChild">appendChild</a></code> and <code><a title="En/DOM/Node.insertBefore" rel="internal" href="https://developer.mozilla.org/En/DOM/Node.insertBefore">insertBefore</a></code>), in which case the children of the fragment are appended or inserted, not the fragment itself.</p><br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/DocumentFragment">MDN</a>. */
@:native("DocumentFragment")
extern class DocumentFragment extends Node
{
    function querySelector (selectors :String) :Element;

    function querySelectorAll (selectors :String) :NodeList;

}
