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

/** The DOM&nbsp;<code>MouseEvent</code> represents events that occur due to the user interacting with a pointing device (such as a mouse). It's represented by the <code><a rel="internal" href="https://developer.mozilla.org/Article_not_found?uri=en/XPCOM_Interface_Reference/nsINSDOMMouseEvent&amp;ident=nsINSDOMMouseEvent" class="new">nsINSDOMMouseEvent</a></code>
&nbsp;interface, which extends the <code><a rel="internal" href="https://developer.mozilla.org/Article_not_found?uri=en/XPCOM_Interface_Reference/nsIDOMMouseEvent&amp;ident=nsIDOMMouseEvent" class="new">nsIDOMMouseEvent</a></code>
 interface.<br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/MouseEvent">MDN</a>. */
@:native("MouseEvent")
extern class MouseEvent extends UIEvent
{
    /** <code>true</code> if the alt key was down when the mouse event was fired. <strong>Read only.</strong> */
    var altKey (default,null) :Bool;

    /** The button number that was pressed when the mouse event was fired:&nbsp;Left button=0, middle button=1 (if present), right button=2. For mice configured for left handed use in which the button actions are reversed the values are instead read from right to left. <strong>Read only.</strong> */
    var button (default,null) :Int;

    /** The X coordinate of the mouse pointer in local (DOM content)&nbsp;coordinates. <strong>Read only.</strong> */
    var clientX (default,null) :Int;

    /** The Y coordinate of the mouse pointer in local (DOM content)&nbsp;coordinates. <strong>Read only.</strong> */
    var clientY (default,null) :Int;

    /** <code>true</code> if the control key was down when the mouse event was fired. <strong>Read only.</strong> */
    var ctrlKey (default,null) :Bool;

    var dataTransfer (default,null) :Clipboard;

    var fromElement (default,null) :Node;

    /** <code>true</code> if the meta key was down when the mouse event was fired. <strong>Read only.</strong> */
    var metaKey (default,null) :Bool;

    var movementX (default,null) :Int;

    var movementY (default,null) :Int;

    var offsetX (default,null) :Int;

    var offsetY (default,null) :Int;

    /** The target to which the event applies. <strong>Read only.</strong> */
    var relatedTarget (default,null) :EventTarget;

    /** The X coordinate of the mouse pointer in global (screen)&nbsp;coordinates. <strong>Read only.</strong> */
    var screenX (default,null) :Int;

    /** The Y coordinate of the mouse pointer in global (screen)&nbsp;coordinates. <strong>Read only.</strong> */
    var screenY (default,null) :Int;

    /** <code>true</code> if the shift key was down when the mouse event was fired. <strong>Read only.</strong> */
    var shiftKey (default,null) :Bool;

    var toElement (default,null) :Node;

    var x (default,null) :Int;

    var y (default,null) :Int;

    function initMouseEvent (type :String, canBubble :Bool, cancelable :Bool, view :DOMWindow, detail :Int, screenX :Int, screenY :Int, clientX :Int, clientY :Int, ctrlKey :Bool, altKey :Bool, shiftKey :Bool, metaKey :Bool, button :Int, relatedTarget :EventTarget) :Void;

}
