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

<a rel="custom" href="http://mxr.mozilla.org/mozilla-central/source/dom/interfaces/threads/nsIDOMWorkers.idl"><code>dom/interfaces/threads/nsIDOMWorkers.idl</code></a><span><a rel="internal" href="https://developer.mozilla.org/en/Interfaces/About_Scriptable_Interfaces" title="en/Interfaces/About_Scriptable_Interfaces">Scriptable</a></span></div><span>This interface represents a worker thread's message port, which is used to allow the worker to post messages back to its creator.</span><div><div>1.0</div><div>11.0</div><div></div><div>Introduced</div><div>Gecko 1.9.1</div><div title="Introduced in Gecko 1.9.1 (Firefox 3.5 / Thunderbird 3.0 / SeaMonkey 2.0)
"></div><div title="Last changed in Gecko 1.9.1 (Firefox 3.5 / Thunderbird 3.0 / SeaMonkey 2.0)
"></div></div>
<div>Inherits from: <code><a rel="custom" href="https://developer.mozilla.org/en/XPCOM_Interface_Reference/nsISupports">nsISupports</a></code>
<span>Last changed in Gecko 1.9.1 (Firefox 3.5 / Thunderbird 3.0 / SeaMonkey 2.0)
</span></div><br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/XPCOM_Interface_Reference/NsIWorkerMessagePort">MDN</a>. */
@:native("MessagePort")
extern class MessagePort extends EventTarget
{
    var onmessage :EventListener;

    function close () :Void;

    function postMessage (message :Dynamic, ?messagePorts :Array<Dynamic>) :Void;

    function start () :Void;

}
