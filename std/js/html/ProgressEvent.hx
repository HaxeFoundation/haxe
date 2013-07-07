/*
 * Copyright (C)2005-2013 Haxe Foundation
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

<a rel="custom" href="http://mxr.mozilla.org/mozilla-central/source/dom/interfaces/events/nsIDOMProgressEvent.idl"><code>dom/interfaces/events/nsIDOMProgressEvent.idl</code></a><span><a rel="internal" href="https://developer.mozilla.org/en/Interfaces/About_Scriptable_Interfaces" title="en/Interfaces/About_Scriptable_Interfaces">Scriptable</a></span></div><span>This interface represents the events sent with progress information while uploading data using the <code>XMLHttpRequest</code> object.</span><div><div>1.0</div><div>11.0</div><div></div><div>Introduced</div><div>Gecko 1.9.1</div><div title="Introduced in Gecko 1.9.1 (Firefox 3.5 / Thunderbird 3.0 / SeaMonkey 2.0)
"></div><div title="Last changed in Gecko 1.9.1 (Firefox 3.5 / Thunderbird 3.0 / SeaMonkey 2.0)
"></div></div>
<div>Inherits from: <code><a rel="custom" href="https://developer.mozilla.org/en/XPCOM_Interface_Reference/nsIDOMEvent">nsIDOMEvent</a></code>
<span>Last changed in Gecko 1.9.1 (Firefox 3.5 / Thunderbird 3.0 / SeaMonkey 2.0)
</span></div></div>
<p></p>
<p>The <code>nsIDOMProgressEvent</code> is used in the media elements (<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/video">&lt;video&gt;</a></code>
 and <code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/audio">&lt;audio&gt;</a></code>
) to inform interested code of the progress of the media download. This implementation is a placeholder until the specification is complete, and is compatible with the WebKit ProgressEvent.</p><br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/XPCOM_Interface_Reference/NsIDOMProgressEvent">MDN</a>. */
@:native("ProgressEvent")
extern class ProgressEvent extends Event
{
	/** Specifies whether or not the total size of the transfer is known. <strong>Read only.</strong> */
	var lengthComputable(default,null) : Bool;

	/** The number of bytes transferred since the beginning of the operation. This doesn't include headers and other overhead, but only the content itself. <strong>Read only.</strong> */
	var loaded(default,null) : Int;

	/** The total number of bytes of content that will be transferred during the operation. If the total size is unknown, this value is zero. <strong>Read only.</strong> */
	var total(default,null) : Int;

	function new(type : String, canBubble : Bool = true, cancelable : Bool = true) : Void;

}
