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

// This file is generated from mozilla\Navigator.webidl. Do not edit!

package js.html;

/**
	The `Navigator` interface represents the state and the identity of the user agent. It allows scripts to query it and to register themselves to carry on some activities.

	Documentation [Navigator](https://developer.mozilla.org/en-US/docs/Web/API/Navigator) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/Navigator$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/Navigator>
**/
@:native("Navigator")
extern class Navigator
{
	
	/**
		Returns a `Permissions` object that can be used to query and update permission status of APIs covered by the Permissions API.
	**/
	var permissions(default,null) : Permissions;
	var mimeTypes(default,null) : MimeTypeArray;
	var plugins(default,null) : PluginArray;
	
	/**
		Reports the value of the user's do-not-track preference. When this value is "yes", your web site or application should not track the user.
	**/
	var doNotTrack(default,null) : String;
	
	/**
		Returns a `BatteryManager` object you can use to get information about the battery charging status.
	**/
	var battery(default,null) : BatteryManager;
	var maxTouchPoints(default,null) : Int;
	
	/**
		Returns a string that represents the current operating system.
	**/
	var oscpu(default,null) : String;
	
	/**
		Returns the vendor name of the current browser (e.g., "Netscape6").
	**/
	var vendor(default,null) : String;
	
	/**
		Returns the vendor version number (e.g. "6.1").
	**/
	var vendorSub(default,null) : String;
	
	/**
		Returns the build number of the current browser (e.g., "20060909").
	**/
	var productSub(default,null) : String;
	
	/**
		Returns a boolean indicating whether cookies are enabled in the browser or not.
	**/
	var cookieEnabled(default,null) : Bool;
	var buildID(default,null) : String;
	var hardwareConcurrency(default,null) : Int;
	
	/**
		Returns a `Geolocation` object allowing accessing the location of the device.
	**/
	var geolocation(default,null) : Geolocation;
	var appCodeName(default,null) : String;
	var appName(default,null) : String;
	var appVersion(default,null) : String;
	var platform(default,null) : String;
	var userAgent(default,null) : String;
	var product(default,null) : String;
	var language(default,null) : String;
	var languages(default,null) : Array<String>;
	var onLine(default,null) : Bool;
	
	/** @throws DOMError */
	function getBattery() : Promise<BatteryManager>;
	@:overload( function( duration : Int ) : Bool {} )
	function vibrate( pattern : Array<Int> ) : Bool;
	/** @throws DOMError */
	function javaEnabled() : Bool;
	/** @throws DOMError */
	function getGamepads() : Array<Gamepad>;
	/** @throws DOMError */
	function sendBeacon( url : String, ?data : haxe.extern.EitherType<ArrayBufferView,haxe.extern.EitherType<Blob,haxe.extern.EitherType<String,FormData>>> ) : Bool;
	/** @throws DOMError */
	function registerProtocolHandler( scheme : String, url : String, title : String ) : Void;
	/** @throws DOMError */
	function registerContentHandler( mimeType : String, url : String, title : String ) : Void;
	function taintEnabled() : Bool;
}