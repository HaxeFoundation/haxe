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

// This file is generated from mozilla\MediaDevices.webidl. Do not edit!

package js.html;

import js.lib.Promise;

/**
	The `MediaDevices` interface provides access to connected media input devices like cameras and microphones, as well as screen sharing. In essence, it lets you obtain access to any hardware source of media data.

	Documentation [MediaDevices](https://developer.mozilla.org/en-US/docs/Web/API/MediaDevices) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/MediaDevices$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/MediaDevices>
**/
@:native("MediaDevices")
extern class MediaDevices extends EventTarget {

	/**
		The event handler for the `devicechange` event. This event is delivered to the `MediaDevices` object when a media input or output device is attached to or removed from the user's computer.
	**/
	var ondevicechange : haxe.Constraints.Function;

	function getSupportedConstraints() : MediaTrackSupportedConstraints;
	/** @throws DOMError */
	function enumerateDevices() : Promise<Array<MediaDeviceInfo>>;
	/** @throws DOMError */
	function getUserMedia( ?constraints : MediaStreamConstraints ) : Promise<MediaStream>;
}