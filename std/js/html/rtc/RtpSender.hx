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

// This file is generated from mozilla\RTCRtpSender.webidl. Do not edit!

package js.html.rtc;

import js.lib.Promise;

/**
	The `RTCRtpSender` interface provides the ability to control and obtain details about how a particular `MediaStreamTrack` is encoded and sent to a remote peer.

	Documentation [RTCRtpSender](https://developer.mozilla.org/en-US/docs/Web/API/RTCRtpSender) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/RTCRtpSender$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/RTCRtpSender>
**/
@:native("RTCRtpSender")
extern class RtpSender {

	/**
		The `MediaStreamTrack` which is being handled by the `RTCRtpSender`. If `track` is `null`, the `RTCRtpSender` doesn't transmit anything.
	**/
	var track(default,null) : js.html.MediaStreamTrack;

	/**
		An `RTCDTMFSender` which can be used to send `DTMF` tones using `"telephone-event"` payloads on the RTP session represented by the `RTCRtpSender` object. If `null`, the track and/or the connection doesn't support DTMF. Only audio tracks can support DTMF.
	**/
	var dtmf(default,null) : DTMFSender;

	function setParameters( ?parameters : RtpParameters ) : Promise<Void>;
	function getParameters() : RtpParameters;
	function replaceTrack( withTrack : js.html.MediaStreamTrack ) : Promise<Void>;
	function getStats() : Promise<StatsReport>;
}