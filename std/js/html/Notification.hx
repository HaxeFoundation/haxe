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

/** <div class="geckoMinversionHeaderTemplate"><p>Mobile Only in Gecko 2.0</p><p>Available only in Firefox Mobile as of Gecko 2.0 (Firefox 4 / Thunderbird 3.3 / SeaMonkey 2.1)
</p></div>

<div><p>Non-standard</p></div><p></p>
<p>The notification object, which you create using the <code><a rel="custom" href="https://developer.mozilla.org/en/DOM/navigator.mozNotification">navigator.mozNotification</a></code>
&nbsp;object's <code>createNotification()</code>&nbsp;method, is used to configure and display desktop notifications to the user.</p><br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/notification">MDN</a>. */
@:native("Notification")
extern class Notification extends EventTarget
{
	var dir : String;

	/** &nbsp;A function to call when the notification is clicked. */
	var onclick : EventListener;

	/** &nbsp;A function to call when the notification is dismissed. */
	var onclose : EventListener;

	var ondisplay : EventListener;

	var onerror : EventListener;

	var onshow : EventListener;

	var permission (default,null) : String;

	var replaceId : String;

	var tag : String;

	function new() : Void;

	function cancel() : Void;

	function close() : Void;

	static function requestPermission( callback_ : NotificationPermissionCallback ) : Void;

	function show() : Void;

}
