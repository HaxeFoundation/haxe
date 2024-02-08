/*
 * Copyright (C)2005-2019 Haxe Foundation
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

import js.lib.Promise;

/**
	The `Navigator` interface represents the state and the identity of the user agent. It allows scripts to query it and to register themselves to carry on some activities.

	Documentation [Navigator](https://developer.mozilla.org/en-US/docs/Web/API/Navigator) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/Navigator$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/Navigator>
**/
@:native("Navigator")
extern class Navigator {

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
		Returns false if setting a cookie will be ignored and true otherwise.
	**/
	var cookieEnabled(default,null) : Bool;

	/**
		Returns the build identifier of the browser (e.g., "2006090803").
	**/
	var buildID(default,null) : String;

	/**
		Returns a reference to a `MediaDevices` object which can then be used to get information about available media devices (`MediaDevices.enumerateDevices()`), find out what constrainable properties are supported for media on the user's computer and user agent (`MediaDevices.getSupportedConstraints()`), and to request access to media using `MediaDevices.getUserMedia()`.
	**/
	var mediaDevices(default,null) : MediaDevices;

	/**
		Returns a `ServiceWorkerContainer` object, which provides access to registration, removal, upgrade, and communication with the `ServiceWorker` objects for the associated document.
	**/
	var serviceWorker(default,null) : ServiceWorkerContainer;
	var clipboard(default,null) : Clipboard;
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
	var storage(default,null) : StorageManager;

	/**
		Returns a `WakeLock` object which allows a document to acquire a screen wake lock.
	**/
	var wakeLock(default,null) : WakeLock;

	@:overload( function( duration : Int ) : Bool {} )
	function vibrate( pattern : Array<Int> ) : Bool;
	function javaEnabled() : Bool;
	/** @throws DOMError */
	function getGamepads() : Array<Gamepad>;
	/** @throws DOMError */
	function requestMIDIAccess( ?options : js.html.midi.MIDIOptions ) : Promise<js.html.midi.MIDIAccess>;
	/** @throws DOMError */
	@:overload( function( url : String, ?data : js.lib.ArrayBufferView) : Bool {} )
	@:overload( function( url : String, ?data : js.lib.ArrayBuffer) : Bool {} )
	@:overload( function( url : String, ?data : FormData) : Bool {} )
	@:overload( function( url : String, ?data : URLSearchParams) : Bool {} )
	@:overload( function( url : String, ?data : String) : Bool {} )
	function sendBeacon( url : String, ?data : Blob ) : Bool;
	function requestMediaKeySystemAccess( keySystem : String, supportedConfigurations : Array<js.html.eme.MediaKeySystemConfiguration> ) : Promise<js.html.eme.MediaKeySystemAccess>;
	function taintEnabled() : Bool;

	/**
		Returns `true` if the equivalent call to `share()` would succeed.
		Returns `false` if the data cannot be validated.
	**/
	function canShare(?data: NavigatorShareData): Bool;

	/**
		Invokes the native sharing mechanism of the device to share data such as text, URLs, or files.
	**/
	function share(data: NavigatorShareData): Promise<Void>;
}

/**
	An object containing data to share.
**/
typedef NavigatorShareData = {

	/**
		An array of `File` objects representing files to be shared.
	**/
	var ?files: Array<File>;

	/**
		A string representing text to be shared.
	**/
	var ?text: String;

	/**
		A string representing the title to be shared.
	**/
	var ?title: String;

	/**
		A string representing a URL to be shared
	**/
	var ?url: String;
}
