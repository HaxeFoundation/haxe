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

/** <div>

<a rel="custom" href="http://mxr.mozilla.org/mozilla-central/source/widget/public/nsIClipboard.idl"><code>widget/public/nsIClipboard.idl</code></a><span><a rel="internal" href="https://developer.mozilla.org/en/Interfaces/About_Scriptable_Interfaces" title="en/Interfaces/About_Scriptable_Interfaces">Scriptable</a></span></div><span>This interface supports basic clipboard operations such as: setting, retrieving, emptying, matching and supporting clipboard data.</span><div>Inherits from: <code><a rel="custom" href="https://developer.mozilla.org/en/XPCOM_Interface_Reference/nsISupports">nsISupports</a></code>
<span>Last changed in Gecko 1.8 (Firefox 1.5 / Thunderbird 1.5 / SeaMonkey 1.0)
</span></div><br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/XPCOM_Interface_Reference/nsIClipboard">MDN</a>. */
@:native("Clipboard")
extern class Clipboard
{
    var dropEffect :String;

    var effectAllowed :String;

    var files (default,null) :FileList;

    var items (default,null) :DataTransferItemList;

    var types (default,null) :Array<Dynamic>;

    function clearData (?type :String) :Void;

    function getData (type :String) :String;

    function setData (type :String, data :String) :Bool;

    function setDragImage (image :ImageElement, x :Int, y :Int) :Void;

}
