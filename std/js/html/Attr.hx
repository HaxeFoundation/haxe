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

/** <p>This type represents a DOM&nbsp;element's attribute as an object. In most DOM methods, you will probably directly retrieve the attribute as a string (e.g., <code><a rel="custom" href="https://developer.mozilla.org/en/DOM/Element.getAttribute">Element.getAttribute()</a></code>
, but certain functions (e.g., <code><a rel="custom" href="https://developer.mozilla.org/en/DOM/Element.getAttributeNode">Element.getAttributeNode()</a></code>
)&nbsp;or means of iterating give <code>Attr</code> types.</p>
<div class="warning"><strong>Warning:</strong> In DOM Core 1, 2 and 3, Attr inherited from Node. This is no longer the case in <a class="external" rel="external" href="http://www.w3.org/TR/dom/" title="http://www.w3.org/TR/dom/" target="_blank">DOM4</a>. In order to bring the implementation of <code>Attr</code> up to specification, work is underway to change it to no longer inherit from <code><a rel="custom" href="https://developer.mozilla.org/en/DOM/Node">Node</a></code>
. You should not be using any <code><a rel="custom" href="https://developer.mozilla.org/en/DOM/Node">Node</a></code>
 properties or methods on <code>Attr</code> objects. Starting in Gecko 7.0 (Firefox 7.0 / Thunderbird 7.0 / SeaMonkey 2.4)
, the ones that are going to be removed output warning messages to the console. You should revise your code accordingly. See <a rel="custom" href="https://developer.mozilla.org/en/DOM/Attr#Deprecated_properties_and_methods">Deprecated properties and methods</a> for a complete list.</div><br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/Attr">MDN</a>. */
@:native("Attr")
extern class Attr extends Node
{
    /** Indicates whether the attribute is an "ID attribute". An "ID attribute" being an attribute which value is expected to be unique across a DOM Document. In HTML DOM, "id" is the only ID attribute, but XML documents could define others. Whether or not an attribute is unique is often determined by a DTD or other schema description. */
    var isId (default,null) :Bool;

    /** The attribute's name. */
    var name (default,null) :String;

    /** This property has been deprecated and will be removed in the future. Since you can only get Attr objects from elements, you should already know th */
    var ownerElement (default,null) :Element;

    /** This property has been deprecated and will be removed in the future; it now always returns <code>true</code>. */
    var specified (default,null) :Bool;

    /** The attribute's value. Setter throws DOMException. */
    var value :String;

}
