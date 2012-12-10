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

/** An <code>EventTarget</code> is a DOM interface implemented by objects that can receive DOM events and have listeners for them. The most common <code>EventTarget</code>s are <a rel="internal" href="https://developer.mozilla.org/en/DOM/element" title="en/DOM/element">DOM elements</a>, although other objects can be <code>EventTarget</code>s too, for example <a rel="internal" href="https://developer.mozilla.org/en/DOM/document" title="en/DOM/document">document</a>, <a rel="internal" href="https://developer.mozilla.org/en/DOM/window" title="en/DOM/window">window</a>, <a rel="internal" href="https://developer.mozilla.org/en/XMLHttpRequest" title="en/XMLHttpRequest">XMLHttpRequest</a>, and others.
<br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/EventTarget">MDN</a>. */
@:native("EventTarget")
extern class EventTarget
{
    function addEventListener (type :String, listener :EventListener, ?useCapture :Bool) :Void;

    function dispatchEvent (event :Event) :Bool;

    function removeEventListener (type :String, listener :EventListener, ?useCapture :Bool) :Void;

}
