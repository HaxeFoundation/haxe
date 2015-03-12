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

// This file is generated from mozilla/Navigator.webidl line 23:0. Do not edit!

package js.html;

@:native("Navigator")
extern class Navigator
{
	var mimeTypes(default,null) : MimeTypeArray;
	var plugins(default,null) : PluginArray;
	var doNotTrack(default,null) : String;
	var maxTouchPoints(default,null) : Int;
	var oscpu(default,null) : String;
	var vendor(default,null) : String;
	var vendorSub(default,null) : String;
	var productSub(default,null) : String;
	var cookieEnabled(default,null) : Bool;
	var buildID(default,null) : String;
	var battery(default,null) : BatteryManager;
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
	
	@:overload( function( duration : Int ) : Bool {} )
	function vibrate( pattern : Array<Int> ) : Bool;
	/** @throws DOMError */
	function javaEnabled() : Bool;
	/** @throws DOMError */
	function getGamepads() : Array<Gamepad>;
	/** @throws DOMError */
	function sendBeacon( url : String, ?data : haxe.extern.EitherType<ArrayBufferView,haxe.extern.EitherType<Blob,haxe.extern.EitherType<String,FormData>>> ) : Bool;
	function requestMediaKeySystemAccess( keySystem : String, ?supportedConfigurations : Array<MediaKeySystemOptions> ) : Promise<MediaKeySystemAccess>;
	/** @throws DOMError */
	function registerProtocolHandler( scheme : String, url : String, title : String ) : Void;
	/** @throws DOMError */
	function registerContentHandler( mimeType : String, url : String, title : String ) : Void;
	function taintEnabled() : Bool;
}