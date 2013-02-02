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

/** Returns a reference to the navigator object, which can be queried for information about the application running the script.<br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/window.navigator">MDN</a>. */
@:native("Navigator")
extern class Navigator
{
	/** Returns the internal "code" name of the current browser. Do not rely on this property to return the correct value. */
	var appCodeName (default,null) : String;

	/** Returns the official name of the browser. Do not rely on this property to return the correct value. */
	var appName (default,null) : String;

	/** Returns the version of the browser as a string. Do not rely on this property to return the correct value. */
	var appVersion (default,null) : String;

	var battery (default,null) : BatteryManager;

	/** Returns a boolean indicating whether cookies are enabled in the browser or not. */
	var cookieEnabled (default,null) : Bool;

	var geolocation (default,null) : Geolocation;

	/** Returns a string representing the language version of the browser. */
	var language (default,null) : String;

	/** Returns a list of the MIME types supported by the browser. */
	var mimeTypes (default,null) : DOMMimeTypeArray;

	/** Returns a boolean indicating whether the browser is working online. */
	var onLine (default,null) : Bool;

	/** Returns a string representing the platform of the browser. */
	var platform (default,null) : String;

	/** Returns an array of the plugins installed in the browser. */
	var plugins (default,null) : DOMPluginArray;

	/** Returns the product name of the current browser. (e.g. "Gecko") */
	var product (default,null) : String;

	/** Returns the build number of the current browser (e.g. "20060909") */
	var productSub (default,null) : String;

	/** Returns the user agent string for the current browser. */
	var userAgent (default,null) : String;

	/** Returns the vendor name of the current browser (e.g. "Netscape6") */
	var vendor (default,null) : String;

	/** Returns the vendor version number (e.g. "6.1") */
	var vendorSub (default,null) : String;

	function getGamepads() : GamepadList;

	function getStorageUpdates() : Void;

	function getUserMedia( options : Dynamic, successCallback : js.html.rtc.NavigatorUserMediaSuccessCallback, ?errorCallback : js.html.rtc.NavigatorUserMediaErrorCallback ) : Void;

	function javaEnabled() : Bool;

}
