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

<a rel="custom" href="http://mxr.mozilla.org/mozilla-central/source/dom/interfaces/events/nsIDOMCompositionEvent.idl"><code>dom/interfaces/events/nsIDOMCompositionEvent.idl</code></a><span><a rel="internal" href="https://developer.mozilla.org/en/Interfaces/About_Scriptable_Interfaces" title="en/Interfaces/About_Scriptable_Interfaces">Scriptable</a></span></div><span>An event interface for composition events</span><div><div>1.0</div><div>11.0</div><div></div><div>Introduced</div><div>Gecko 9.0</div><div title="Introduced in Gecko 9.0 (Firefox 9.0 / Thunderbird 9.0 / SeaMonkey 2.6)
"></div><div title="Last changed in Gecko 9.0 (Firefox 9.0 / Thunderbird 9.0 / SeaMonkey 2.6)
"></div></div>
<div>Inherits from: <code><a rel="internal" href="https://developer.mozilla.org/Article_not_found?uri=en/XPCOM_Interface_Reference/nsIDOMUIEvent&amp;ident=nsIDOMUIEvent" class="new">nsIDOMUIEvent</a></code>
<span>Last changed in Gecko 9.0 (Firefox 9.0 / Thunderbird 9.0 / SeaMonkey 2.6)
</span></div></div>
<p></p>
<p>The DOM <code>CompositionEvent</code> represents events that occur due to the user indirectly entering text.</p><br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/CompositionEvent">MDN</a>. */
@:native("CompositionEvent")
extern class CompositionEvent extends UIEvent
{
	/** <p>For <code><a rel="custom" href="https://developer.mozilla.org/en/DOM/DOM_event_reference/compositionstart">compositionstart</a></code>
 events, this is the currently selected text that will be replaced by the string being composed. This value doesn't change even if content changes the selection range; rather, it indicates the string that was selected when composition started.</p> <p>For <code><a rel="custom" href="https://developer.mozilla.org/en/DOM/DOM_event_reference/compositionupdate">compositionupdate</a></code>
, this is the string as it stands currently as editing is ongoing.</p> <p>For <code><a rel="custom" href="https://developer.mozilla.org/en/DOM/DOM_event_reference/compositionend">compositionend</a></code>
 events, this is the string as committed to the editor.</p> <p><strong>Read only</strong>.</p> */
	var data(default,null) : String;

	function initCompositionEvent( typeArg : String, canBubbleArg : Bool, cancelableArg : Bool, viewArg : DOMWindow, dataArg : String ) : Void;

}
