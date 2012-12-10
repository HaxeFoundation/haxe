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

/** Returns a reference to the <code>History</code> object, which provides an interface for manipulating the browser <em>session history</em> (pages visited in the tab or frame that the current page is loaded in).<br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/window.history">MDN</a>. */
@:native("History")
extern class History
{
    /** Read-only. Returns the number of elements in the session history, including the currently loaded page. For example, for a page loaded in a new tab this property returns <code>1</code>. */
    var length (default,null) :Int;

    /** Returns the state at the top of the history stack. This is a way to look at the state without having to wait for a <code>popstate</code> event. <strong>Read only.</strong> */
    var state (default,null) :Dynamic;

    function back () :Void;

    function forward () :Void;

    function go (distance :Int) :Void;

    function pushState (data :Dynamic, title :String, ?url :String) :Void;

    function replaceState (data :Dynamic, title :String, ?url :String) :Void;

}
