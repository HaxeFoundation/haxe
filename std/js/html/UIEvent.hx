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

/** <div><div>

<a rel="custom" href="http://mxr.mozilla.org/mozilla-central/source/dom/interfaces/events/nsIDOMUIEvent.idl"><code>dom/interfaces/events/nsIDOMUIEvent.idl</code></a><span><a rel="internal" href="https://developer.mozilla.org/en/Interfaces/About_Scriptable_Interfaces" title="en/Interfaces/About_Scriptable_Interfaces">Scriptable</a></span></div><span>A basic event interface for all user interface events</span><div><div>1.0</div><div>11.0</div><div title="Introduced in Gecko 1.0 
"></div><div title="Last changed in Gecko 9.0 
"></div></div>
<div>Inherits from: <code><a rel="custom" href="https://developer.mozilla.org/en/XPCOM_Interface_Reference/nsIDOMEvent">nsIDOMEvent</a></code>
<span>Last changed in Gecko 9.0 (Firefox 9.0 / Thunderbird 9.0 / SeaMonkey 2.6)
</span></div></div>
<p></p>
<p>The DOM <code>UIEvent</code> represents simple user interface events.</p><br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/UIEvent">MDN</a>. */
@:native("UIEvent")
extern class UIEvent extends Event
{
    var charCode (default,null) :Int;

    /** Detail about the event, depending on the type of event. <strong>Read only.</strong> */
    var detail (default,null) :Int;

    var keyCode (default,null) :Int;

    var layerX (default,null) :Int;

    var layerY (default,null) :Int;

    var pageX (default,null) :Int;

    var pageY (default,null) :Int;

    /** A view which generated the event. <strong>Read only.</strong> */
    var view (default,null) :DOMWindow;

    var which (default,null) :Int;

    function initUIEvent (type :String, canBubble :Bool, cancelable :Bool, view :DOMWindow, detail :Int) :Void;

}
