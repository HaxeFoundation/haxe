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

/** <p>Workers are background tasks that can be easily created and can send messages back to their creators. Creating a worker is as simple as calling the <code>Worker()</code>&nbsp;constructor, specifying a script to be run in the worker thread.</p>
<p>Of note is the fact that workers may in turn spawn new workers as long as those workers are hosted within the same origin as the parent page.&nbsp; In addition, workers may use <a title="En/XMLHttpRequest" class="internal" rel="internal" href="https://developer.mozilla.org/en/DOM/XMLHttpRequest"><code>XMLHttpRequest</code></a> for network I/O, with the exception that the <code>responseXML</code> and <code>channel</code> attributes on <code>XMLHttpRequest</code> always return <code>null</code>.</p>
<p>For a list of global functions available to workers, see <a title="En/DOM/Worker/Functions available to workers" rel="internal" href="https://developer.mozilla.org/En/DOM/Worker/Functions_available_to_workers">Functions available to workers</a>.</p>
<div class="geckoVersionNote">
<p>
</p><div class="geckoVersionHeading">Gecko 2.0 note<div>(Firefox 4 / Thunderbird 3.3 / SeaMonkey 2.1)
</div></div>
<p></p>
<p>If you want to use workers in extensions, and would like to have access to <a title="en/js-ctypes" rel="internal" href="https://developer.mozilla.org/en/js-ctypes">js-ctypes</a>, you should use the <code><a rel="custom" href="https://developer.mozilla.org/en/DOM/ChromeWorker">ChromeWorker</a></code>
 object instead.</p>
</div>
<p>See <a class="internal" title="en/Using DOM workers" rel="internal" href="https://developer.mozilla.org/En/Using_web_workers">Using web workers</a> for examples and details.</p><br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/En/DOM/Worker">MDN</a>. */
@:native("Worker")
extern class Worker extends AbstractWorker
{
    /** An event listener that is called whenever a <code>MessageEvent</code> with type <code>message</code> bubbles through the worker. The message is stored in the event's <code>data</code> member. */
    var onmessage :EventListener;

    /** <p>The constructor creates a web worker that executes the script at the specified URL. This script must obey the <a title="Same origin policy for JavaScript" rel="internal" href="https://developer.mozilla.org/en/Same_origin_policy_for_JavaScript">same-origin policy</a>. Note that there is a disagreement among browser manufacturers about whether a data URI is of the same origin or not. Though Gecko 10.0 (Firefox 10.0 / Thunderbird 10.0)
 and later accept data URIs, that's not the case in all other browsers.</p>
<pre>Worker(
&nbsp;&nbsp;in DOMString aStringURL
);
</pre>
<div id="section_6"><span id="Parameters"></span><h5 class="editable">Parameters</h5>
<dl> <dt><code>aStringURL</code></dt> <dd>The URL of the script the worker is to execute. It must obey the same-origin policy (or be a data URI for Gecko 10.0 or later).</dd>
</dl>
<div id="section_7"><span id="Return_value"></span><h6 class="editable">Return value</h6>
<p>A new <code>Worker</code>.</p>
</div></div> */
    function new () :Void;

    function postMessage (message :Dynamic, ?messagePorts :Array<Dynamic>) :Void;

    function terminate () :Void;

}
