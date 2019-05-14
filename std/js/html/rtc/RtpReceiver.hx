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

// This file is generated from mozilla\RTCRtpReceiver.webidl. Do not edit!

package js.html.rtc;

import js.lib.Promise;

/**
	The `RTCRtpReceiver` interface of the the WebRTC API manages the reception and decoding of data for a `MediaStreamTrack` on an `RTCPeerConnection`.

	Documentation [RTCRtpReceiver](https://developer.mozilla.org/en-US/docs/Web/API/RTCRtpReceiver) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/RTCRtpReceiver$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/RTCRtpReceiver>
**/
@:native("RTCRtpReceiver")
extern class RtpReceiver {

	/**
		Returns the `MediaStreamTrack` associated with the current `RTCRtpReceiver` instance. 
	**/
	var track(default,null) : js.html.MediaStreamTrack;


	/**
		Returns a `Promise` whose fulfillment handler receives a `RTCStatsReport` which contains statistics about the incoming streams and their dependencies.
	**/
	function getStats() : Promise<StatsReport>;

	/**
		Returns an array of `RTCRtpContributingSource` instances for each unique CSRC (contributing source) identifier received by the current `RTCRtpReceiver` in the last ten seconds.
	**/
	function getContributingSources() : Array<RtpContributingSource>;

	/**
		Returns an array including one `RTCRtpSynchronizationSource` instance for each unique SSRC (synchronization source) identifier received by the current `RTCRtpReceiver` in the last ten seconds.
	**/
	function getSynchronizationSources() : Array<RtpSynchronizationSource>;
}