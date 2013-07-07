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

/** <p>The <code>EventSource</code> interface is used to manage server-sent events. You can set the onmessage attribute to a JavaScript function to receive non-typed messages (that is, messages with no <code>event</code> field). You can also call <code>addEventListener()</code> to listen for events just like any other event source.</p>
<p>See <a title="en/Server-sent events/Using server-sent events" rel="internal" href="https://developer.mozilla.org/en/Server-sent_events/Using_server-sent_events">Using server-sent events</a> for further details.</p><br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/Server-sent_events/EventSource">MDN</a>. */
@:native("EventSource")
extern class EventSource extends EventTarget
{
	/** The connection is not being established, has been closed or there was a fatal error. */
	static inline var CLOSED : Int = 2;

	/** The connection is being established. */
	static inline var CONNECTING : Int = 0;

	/** The connection is open and dispatching events. */
	static inline var OPEN : Int = 1;

	var URL(default,null) : String;

	/** A JavaScript function to call when an error occurs. */
	var onerror : EventListener;

	/** A JavaScript function to call when an a message without an <code>event</code> field arrives. */
	var onmessage : EventListener;

	/** A JavaScript function to call when the connection has opened. */
	var onopen : EventListener;

	/** The state of the connection, must be one of <code>CONNECTING</code>, <code>OPEN</code>, or <code>CLOSED</code>. <strong>Read only.</strong> */
	var readyState(default,null) : Int;

	/** Read only. */
	var url(default,null) : String;

	function new() : Void;

	function close() : Void;

}
