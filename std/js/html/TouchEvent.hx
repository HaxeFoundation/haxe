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

/** <p>A <code>TouchEvent</code> represents an event sent when the state of contacts with a touch-sensitive surface changes. This surface can be a touch screen or trackpad, for example. The event can describe one or more points of contact with the screen and includes support for detecting movement, addition and removal of contact points, and so forth.</p>
<p>Touches are represented by the <code><a rel="custom" href="https://developer.mozilla.org/en/DOM/Touch">Touch</a></code>
&nbsp;object; each touch is described by a position, size and shape, amount of pressure, and target element. Lists of touches are represented by <code><a rel="custom" href="https://developer.mozilla.org/en/DOM/TouchList">TouchList</a></code>
 objects.</p><br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/TouchEvent">MDN</a>. */
@:native("TouchEvent")
extern class TouchEvent extends UIEvent
{
    /** A Boolean value indicating whether or not the alt key was down when the touch event was fired. <strong>Read only.</strong> */
    var altKey (default,null) :Bool;

    /** A <code><a rel="custom" href="https://developer.mozilla.org/en/DOM/TouchList">TouchList</a></code>
 of all the <code><a rel="custom" href="https://developer.mozilla.org/en/DOM/Touch">Touch</a></code>
 objects representing individual points of contact whose states changed between the previous touch event and this one. <strong>Read only.</strong> */
    var changedTouches (default,null) :TouchList;

    /** A Boolean value indicating whether or not the control key was down when the touch event was fired. <strong>Read only.</strong> */
    var ctrlKey (default,null) :Bool;

    /** A Boolean value indicating whether or not the meta key was down when the touch event was fired. <strong>Read only.</strong> */
    var metaKey (default,null) :Bool;

    /** A Boolean value indicating whether or not the shift key was down when the touch event was fired. <strong>Read only.</strong> */
    var shiftKey (default,null) :Bool;

    /** A <code><a rel="custom" href="https://developer.mozilla.org/en/DOM/TouchList">TouchList</a></code>
 of all the <code><a rel="custom" href="https://developer.mozilla.org/en/DOM/Touch">Touch</a></code>
&nbsp;objects that are both currently in contact with the touch surface <strong>and</strong> were also started on the same element that is the target of the event. <strong>Read only.</strong> */
    var targetTouches (default,null) :TouchList;

    /** A <code><a rel="custom" href="https://developer.mozilla.org/en/DOM/TouchList">TouchList</a></code>
 of all the <code><a rel="custom" href="https://developer.mozilla.org/en/DOM/Touch">Touch</a></code>
&nbsp;objects representing all current points of contact with the surface, regardless of target or changed status. <strong>Read only.</strong> */
    var touches (default,null) :TouchList;

    function initTouchEvent (touches :TouchList, targetTouches :TouchList, changedTouches :TouchList, type :String, view :DOMWindow, screenX :Int, screenY :Int, clientX :Int, clientY :Int, ctrlKey :Bool, altKey :Bool, shiftKey :Bool, metaKey :Bool) :Void;

}
