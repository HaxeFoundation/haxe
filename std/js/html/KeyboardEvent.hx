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

/** <div class="deprecatedHeaderTemplate"><p>Deprecated</p></div>
<p></p>
<p><code>KeyboardEvent</code> objects describe a user interaction with the keyboard. Each event describes a key; the event type (<code>keydown</code>, <code>keypress</code>, or <code>keyup</code>) identifies what kind of activity was performed.</p>
<div class="note"><strong>Note:</strong> The <code>KeyboardEvent</code> interface is deprecated in DOM&nbsp;Level 3 in favor of the new <code><a rel="internal" href="https://developer.mozilla.org/Article_not_found?uri=en/DOM/TextInput" class="new">TextInput</a></code>
 interface and the corresponding <code>textinput</code> event, which have improved support for alternate input methods.&nbsp; However, DOM Level 3 <code>textinput</code> events are <a title="https://bugzilla.mozilla.org/show_bug.cgi?id=622245" class=" link-https" rel="external" href="https://bugzilla.mozilla.org/show_bug.cgi?id=622245" target="_blank">not yet implemented</a> in Gecko (as of version 6.0), so code written for Gecko browsers should continue to use <code>KeyboardEvent</code> for now.</div><br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/KeyboardEvent">MDN</a>. */
@:native("KeyboardEvent")
extern class KeyboardEvent extends UIEvent
{
    var altGraphKey (default,null) :Bool;

    /** <code>true</code> if the Alt (or Option, on Mac) key was active when the key event was generated. <strong>Read only.</strong> */
    var altKey (default,null) :Bool;

    /** <code>true</code> if the Control key was active when the key event was generated. <strong>Read only.</strong> */
    var ctrlKey (default,null) :Bool;

    var keyIdentifier (default,null) :String;

    var keyLocation (default,null) :Int;

    /** <code>true</code> if the Meta (or Command, on Mac) key was active when the key event was generated. <strong>Read only.</strong> */
    var metaKey (default,null) :Bool;

    /** <code>true</code> if the Shift key was active when the key event was generated. <strong>Read only.</strong> */
    var shiftKey (default,null) :Bool;

    function initKeyboardEvent (type :String, canBubble :Bool, cancelable :Bool, view :DOMWindow, keyIdentifier :String, keyLocation :Int, ctrlKey :Bool, altKey :Bool, shiftKey :Bool, metaKey :Bool, altGraphKey :Bool) :Void;

}
