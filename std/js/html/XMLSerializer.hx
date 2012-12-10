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

/** <div dir="ltr" id="result_box">XMLSerializer can be used to convert DOM subtree or DOM document into text. XMLSerializer is available to unprivileged scripts.</div>
<p><code> </code></p>
<div class="note">
<div dir="ltr">XMLSerializer is mainly useful for applications and extensions based on the Mozilla platform. While it is available for web pages, it's not part of any standard and level of support in other browsers unknown.</div>
<div id="result_box" dir="ltr">&nbsp;</div>
</div><br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/XMLSerializer">MDN</a>. */
@:native("XMLSerializer")
extern class XMLSerializer
{
    function new () :Void;

    function serializeToString (node :Node) :String;

}
