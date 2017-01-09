/*
 * Copyright (C)2005-2017 Haxe Foundation
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

// This file is generated from mozilla\Notification.webidl. Do not edit!

package js.html;

/**
	The `Notification` interface of the Notifications API is used to configure and display desktop notifications to the user.

	Documentation [Notification](https://developer.mozilla.org/en-US/docs/Web/API/Notification) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/Notification$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/Notification>
**/
@:native("Notification")
extern class Notification extends EventTarget
{
	static 
	/**
		A string representing the current permission to display notifications. Possible value are: `denied` (the user refuses to have notifications displayed), `granted` (the user accepts having notifications displayed), or `default` (the user choice is unknown and therefore the browser will act as if the value were denied).
	**/
	var permission(default,null) : NotificationPermission;
	
	
	/**
		A handler for the `click` event. It is triggered each time the user clicks on the notification.
	**/
	var onclick : haxe.Constraints.Function;
	
	/**
		A handler for the `show` event. It is triggered when the notification is displayed.
	**/
	var onshow : haxe.Constraints.Function;
	
	/**
		A handler for the `error` event. It is triggered each time the notification encounters an error.
	**/
	var onerror : haxe.Constraints.Function;
	
	/**
		A handler for the `close` event. It is triggered when the user closes the notification.
	**/
	var onclose : haxe.Constraints.Function;
	
	/**
		The title of the notification as specified in the first parameter of the constructor.
	**/
	var title(default,null) : String;
	
	/**
		The text direction of the notification as specified in the options parameter of the constructor.
	**/
	var dir(default,null) : NotificationDirection;
	
	/**
		The language code of the notification as specified in the options parameter of the constructor.
	**/
	var lang(default,null) : String;
	
	/**
		The body string of the notification as specified in the options parameter of the constructor.
	**/
	var body(default,null) : String;
	
	/**
		The ID of the notification (if any) as specified in the options parameter of the constructor.
	**/
	var tag(default,null) : String;
	
	/**
		The URL of the image used as an icon of the notification as specified in the options parameter of the constructor.
	**/
	var icon(default,null) : String;
	
	/**
		Returns a structured clone of the notification’s data.
	**/
	var data(default,null) : Dynamic;
	
	/** @throws DOMError */
	function new( title : String, ?options : NotificationOptions ) : Void;
	
	/**
		Programmatically closes a notification.
	**/
	function close() : Void;
}