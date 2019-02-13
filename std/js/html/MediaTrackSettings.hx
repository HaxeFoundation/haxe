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

// This file is generated from mozilla\MediaTrackSettings.webidl. Do not edit!

package js.html;

/**
	The `MediaTrackSettings` dictionary is used to return the current values configured for each of a `MediaStreamTrack`'s settings. These values will adhere as closely as possible to any constraints previously described using a `MediaTrackConstraints` object and set using `applyConstraints()`, and will adhere to the default constraints for any properties whose constraints haven't been changed, or whose customized constraints couldn't be matched.

	Documentation [MediaTrackSettings](https://developer.mozilla.org/en-US/docs/Web/API/MediaTrackSettings) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/MediaTrackSettings$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/MediaTrackSettings>
**/
typedef MediaTrackSettings = {
	
	/**
		A Boolean which indicates the current value of the `MediaTrackConstraints.autoGainControl` property, which is `true` if automatic gain control is enabled and is `false` otherwise.
	**/
	var ?autoGainControl : Bool;
	var ?browserWindow : Int;
	
	/**
		A long integer value indicating the current value of the ``MediaTrackConstraints.channelCount`` property, specifying the number of audio channels present on the track (therefore indicating how many audio samples exist in each audio frame). This is 1 for mono, 2 for stereo, and so forth.
	**/
	var ?channelCount : Int;
	
	/**
		A `DOMString` indicating the current value of the ``MediaTrackConstraints.deviceId`` property. The device ID is a origin-unique string identifying the source of the track; this is usually a `GUID`. This value is specific to the source of the track's data and is not usable for setting constraints; it can, however, be used for initially selecting media when calling `MediaDevices.getUserMedia()`.
	**/
	var ?deviceId : String;
	
	/**
		A Boolean indicating the current value of the ``MediaTrackConstraints.echoCancellation`` property, specifying `true` if echo cancellation is enabled, otherwise `false`.
	**/
	var ?echoCancellation : Bool;
	
	/**
		
	**/
	var ?facingMode : String;
	
	/**
		A double-precision floating point value indicating the current value of the ``MediaTrackConstraints.frameRate`` property, specifying how many frames of video per second the track includes. If the value can't be determined for any reason, the value will match the vertical sync rate of the device the user agent is running on.
	**/
	var ?frameRate : Float;
	
	/**
		A long integer value indicating the current value of the ``MediaTrackConstraints.height`` property, specifying the height of the track's video data in pixels.
	**/
	var ?height : Int;
	var ?mediaSource : String;
	
	/**
		A Boolean which indicates the current value of the `MediaTrackConstraints.noiseSuppression` property, which is `true` if noise suppression is enabled and is `false` otherwise.
	**/
	var ?noiseSuppression : Bool;
	var ?scrollWithPage : Bool;
	var ?viewportHeight : Int;
	var ?viewportOffsetX : Int;
	var ?viewportOffsetY : Int;
	var ?viewportWidth : Int;
	
	/**
		A long integer value indicating the current value of the `MediaTrackSettings.width` property, specifying the width of the track's video data in pixels.
	**/
	var ?width : Int;
}