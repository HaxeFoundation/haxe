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

<a rel="custom" href="http://mxr.mozilla.org/mozilla-central/source/dom/interfaces/storage/nsIDOMStorageEvent.idl"><code>dom/interfaces/storage/nsIDOMStorageEvent.idl</code></a><span><a rel="internal" href="https://developer.mozilla.org/en/Interfaces/About_Scriptable_Interfaces" title="en/Interfaces/About_Scriptable_Interfaces">Scriptable</a></span></div><span>Describes an event occurring on HTML5 client-side storage data.</span><div><div>1.0</div><div>11.0</div><div></div><div>Introduced</div><div>Gecko 2.0</div><div title="Introduced in Gecko 2.0 (Firefox 4 / Thunderbird 3.3 / SeaMonkey 2.1)
"></div><div title="Last changed in Gecko 2.0 (Firefox 4 / Thunderbird 3.3 / SeaMonkey 2.1)
"></div></div>
<div>Inherits from: <code><a rel="custom" href="https://developer.mozilla.org/en/XPCOM_Interface_Reference/nsIDOMEvent">nsIDOMEvent</a></code>
<span>Last changed in Gecko 2.0 (Firefox 4 / Thunderbird 3.3 / SeaMonkey 2.1)
</span></div></div>
<p></p>
<p>A <code>StorageEvent</code> is sent to a window when a storage area changes.</p>
<div class="geckoVersionNote">
<p>
</p><div class="geckoVersionHeading">Gecko 2.0 note<div>(Firefox 4 / Thunderbird 3.3 / SeaMonkey 2.1)
</div></div>
<p></p>
<p>Although this event existed prior to Gecko 2.0 (Firefox 4 / Thunderbird 3.3 / SeaMonkey 2.1)
, it did not match the specification. The old event format is now represented by the <code><a rel="custom" href="https://developer.mozilla.org/en/XPCOM_Interface_Reference/nsIDOMStorageEventObsolete">nsIDOMStorageEventObsolete</a></code>
 interface.</p>
</div><br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/event/StorageEvent">MDN</a>. */
@:native("StorageEvent")
extern class StorageEvent extends Event
{
    /** Represents the key changed. The <code>key</code> attribute is <code>null</code> when the change is caused by the storage <code>clear()</code> method. <strong>Read only.</strong> */
    var key (default,null) :String;

    /** The new value of the <code>key</code>. The <code>newValue</code> is <code>null</code> when the change has been invoked by storage <code>clear()</code> method or the <code>key</code> has been removed from the storage. <strong>Read only.</strong> */
    var newValue (default,null) :String;

    /** The original value of the <code>key</code>. The <code>oldValue</code> is <code>null</code> when the change has been invoked by storage <code>clear()</code> method or the <code>key</code> has been newly added and therefor doesn't have any previous value. <strong>Read only.</strong> */
    var oldValue (default,null) :String;

    /** Represents the Storage object that was affected. <strong>Read only.</strong> */
    var storageArea (default,null) :Storage;

    /** The URL of the document whose <code>key</code> changed. <strong>Read only.</strong> */
    var url (default,null) :String;

    function new (type :String, canBubble :Bool = true, cancelable :Bool = true) :Void;

    function initStorageEvent (typeArg :String, canBubbleArg :Bool, cancelableArg :Bool, keyArg :String, oldValueArg :String, newValueArg :String, urlArg :String, storageAreaArg :Storage) :Void;

}
