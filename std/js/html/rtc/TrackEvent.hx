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

// This file is generated from mozilla\RTCTrackEvent.webidl. Do not edit!

package js.html.rtc;

/**
	The WebRTC API interface `RTCTrackEvent` represents the `track` event, which is sent when a new `MediaStreamTrack` is added to an `RTCRtpReceiver` which is part of the `RTCPeerConnection`.

	Documentation [RTCTrackEvent](https://developer.mozilla.org/en-US/docs/Web/API/RTCTrackEvent) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/RTCTrackEvent$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/RTCTrackEvent>
**/
@:native("RTCTrackEvent")
extern class TrackEvent extends js.html.Event {
	
	/**
		The `RTCRtpReceiver` used by the track that's been added to the `RTCPeerConnection`.
	**/
	var receiver(default,null) : RtpReceiver;
	
	/**
		The `MediaStreamTrack` which has been added to the connection.
	**/
	var track(default,null) : js.html.MediaStreamTrack;
	
	/**
		An array of `MediaStream` objects, each representing one of the media streams which comprise the `RTCTrackEvent.track` that was added to the connection. By default, the array is empty.
	**/
	var streams(default,null) : Array<js.html.MediaStream>;
	
	/**
		The `RTCRtpTransceiver` being used by the new track.
	**/
	var transceiver(default,null) : RtpTransceiver;
	
	/** @throws DOMError */
	function new( type : String, eventInitDict : TrackEventInit ) : Void;
}