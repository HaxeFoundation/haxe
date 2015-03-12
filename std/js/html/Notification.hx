/*
 * Copyright (C)2005-2015 Haxe Foundation
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

// This file is generated from mozilla/Notification.webidl line 16:0. Do not edit!

package js.html;

@:native("Notification")
extern class Notification extends EventTarget
{
	static var permission(default,null) : NotificationPermission;
	
	/** @throws DOMError */
	static function requestPermission( ?permissionCallback : NotificationPermission -> Void ) : Void;
	/** @throws DOMError */
	static function get( ?filter : GetNotificationOptions ) : Promise<Array<Notification>>;
	var onclick : haxe.Constraints.Function;
	var onshow : haxe.Constraints.Function;
	var onerror : haxe.Constraints.Function;
	var onclose : haxe.Constraints.Function;
	var title(default,null) : String;
	var dir(default,null) : NotificationDirection;
	var lang(default,null) : String;
	var body(default,null) : String;
	var tag(default,null) : String;
	var icon(default,null) : String;
	var data(default,null) : Dynamic;
	
	/** @throws DOMError */
	function new( title : String, ?options : NotificationOptions ) : Void;
	function close() : Void;
}