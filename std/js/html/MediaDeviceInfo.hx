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

// This file is generated from mozilla\MediaDeviceInfo.webidl. Do not edit!

package js.html;

/**
	The `MediaDevicesInfo` interface contains information that describes a single media input or output device.

	Documentation [MediaDeviceInfo](https://developer.mozilla.org/en-US/docs/Web/API/MediaDeviceInfo) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/MediaDeviceInfo$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/MediaDeviceInfo>
**/
@:native("MediaDeviceInfo")
extern class MediaDeviceInfo {
	
	/**
		Returns a `DOMString` that is an identifier for the represented device that is persisted across sessions. It is un-guessable by other applications and unique to the origin of the calling application. It is reset when the user clears cookies (for Private Browsing, a different identifier is used that is not persisted across sessions).
	**/
	var deviceId(default,null) : String;
	
	/**
		Returns an enumerated value that is either `"videoinput"`, `"audioinput"` or `"audiooutput"`.
	**/
	var kind(default,null) : MediaDeviceKind;
	
	/**
		Returns a `DOMString` that is a label describing this device (for example "External USB Webcam").
	**/
	var label(default,null) : String;
	
	/**
		Returns a `DOMString` that is a group identifier. Two devices have the same group identifier if they belong to the same physical device — for example a monitor with both a built-in camera and a microphone.
	**/
	var groupId(default,null) : String;
	
	function toJSON() : Dynamic;
}