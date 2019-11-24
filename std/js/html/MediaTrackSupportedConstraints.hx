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

// This file is generated from mozilla\MediaTrackSupportedConstraints.webidl. Do not edit!

package js.html;

/**
	The `MediaTrackSupportedConstraints` dictionary establishes the list of constrainable properties recognized by the user agent or browser in its implementation of the `MediaStreamTrack` object. An object conforming to `MediaTrackSupportedConstraints` is returned by `MediaDevices.getSupportedConstraints()`.

	Documentation [MediaTrackSupportedConstraints](https://developer.mozilla.org/en-US/docs/Web/API/MediaTrackSupportedConstraints) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/MediaTrackSupportedConstraints$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/MediaTrackSupportedConstraints>
**/
typedef MediaTrackSupportedConstraints = {
	
	/**
		A Boolean value whose value is `true` if the `aspectRatio` constraint is supported in the current environment.
	**/
	var ?aspectRatio : Bool;
	
	/**
		A Boolean whose value is `true` if the `autoGainControl` constraint is supported in the current environment.
	**/
	var ?autoGainControl : Bool;
	var ?browserWindow : Bool;
	
	/**
		A Boolean value whose value is `true` if the `channelCount` constraint is supported in the current environment.
	**/
	var ?channelCount : Bool;
	
	/**
		A Boolean value whose value is `true` if the `deviceId` constraint is supported in the current environment.
	**/
	var ?deviceId : Bool;
	
	/**
		A Boolean value whose value is `true` if the `echoCancellation` constraint is supported in the current environment.
	**/
	var ?echoCancellation : Bool;
	
	/**
		A Boolean value whose value is `true` if the `facingMode` constraint is supported in the current environment.
	**/
	var ?facingMode : Bool;
	
	/**
		A Boolean value whose value is `true` if the `frameRate` constraint is supported in the current environment.
	**/
	var ?frameRate : Bool;
	
	/**
		A Boolean value whose value is `true` if the `groupId` constraint is supported in the current environment.
	**/
	var ?groupId : Bool;
	
	/**
		A Boolean value whose value is `true` if the `height` constraint is supported in the current environment.
	**/
	var ?height : Bool;
	
	/**
		A Boolean value whose value is `true` if the `latency` constraint is supported in the current environment.
	**/
	var ?latency : Bool;
	var ?mediaSource : Bool;
	
	/**
		A Boolean whose value is `true` if the `noiseSuppression` constraint is supported in the current environment.
	**/
	var ?noiseSuppression : Bool;
	
	/**
		A Boolean value whose value is `true` if the `sampleRate` constraint is supported in the current environment.
	**/
	var ?sampleRate : Bool;
	
	/**
		A Boolean value whose value is `true` if the `sampleSize` constraint is supported in the current environment.
	**/
	var ?sampleSize : Bool;
	var ?scrollWithPage : Bool;
	var ?viewportHeight : Bool;
	var ?viewportOffsetX : Bool;
	var ?viewportOffsetY : Bool;
	var ?viewportWidth : Bool;
	
	/**
		A Boolean value whose value is `true` if the `volume` constraint is supported in the current environment.
	**/
	var ?volume : Bool;
	
	/**
		A Boolean value whose value is `true` if the `width` constraint is supported in the current environment.
	**/
	var ?width : Bool;
}