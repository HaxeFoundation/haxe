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
package js.html.svg;

/** Interface <code>SVGTests</code> defines an interface which applies to all elements which have attributes 
<code><a rel="custom" href="https://developer.mozilla.org/en/SVG/Attribute/requiredFeatures">requiredFeatures</a></code>, 
<code><a rel="internal" href="https://developer.mozilla.org/en/SVG/Attribute/requiredExtensions" class="new">requiredExtensions</a></code> and 
<code><a rel="internal" href="https://developer.mozilla.org/en/SVG/Attribute/systemLanguage" class="new">systemLanguage</a></code>.<br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/SVGTests">MDN</a>. */
@:native("SVGTests")
extern class Tests
{
    /** Corresponds to attribute 
<code><a rel="internal" href="https://developer.mozilla.org/en/SVG/Attribute/requiredExtensions" class="new">requiredExtensions</a></code> on the given element. */
    var requiredExtensions (default,null) :StringList;

    /** Corresponds to attribute 
<code><a rel="custom" href="https://developer.mozilla.org/en/SVG/Attribute/requiredFeatures">requiredFeatures</a></code> on the given element. */
    var requiredFeatures (default,null) :StringList;

    /** Corresponds to attribute 
<code><a rel="internal" href="https://developer.mozilla.org/en/SVG/Attribute/systemLanguage" class="new">systemLanguage</a></code> on the given element. */
    var systemLanguage (default,null) :StringList;

    function hasExtension (extension :String) :Bool;

}
