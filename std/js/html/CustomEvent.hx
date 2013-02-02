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

/** The DOM <code>CustomEvent</code> are events initialized by an application for any purpose. It's represented by the <code><a rel="internal" href="https://developer.mozilla.org/Article_not_found?uri=en/XPCOM_Interface_Reference/nsIDOMCustomEvent&amp;ident=nsIDOMCustomEvent" class="new">nsIDOMCustomEvent</a></code>
&nbsp;interface, which extends the <code><a rel="custom" href="https://developer.mozilla.org/en/XPCOM_Interface_Reference/nsIDOMEvent">nsIDOMEvent</a></code>
 interface.<br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/CustomEvent">MDN</a>. */
@:native("CustomEvent")
extern class CustomEvent extends Event
{
	/** The data passed when initializing the event. */
	var detail (default,null) : Dynamic;

	function new(type : String, canBubble : Bool = true, cancelable : Bool = true) : Void;

	function initCustomEvent( typeArg : String, canBubbleArg : Bool, cancelableArg : Bool, detailArg : Dynamic ) : Void;

}
