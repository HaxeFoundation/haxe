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

// This file is generated from mozilla\RTCRtpTransceiver.webidl. Do not edit!

package js.html.rtc;

/**
	The WebRTC interface `RTCRtpTransceiver` describes a permanent pairing of an `RTCRtpSender` and an `RTCRtpReceiver`, along with some shared state.

	Documentation [RTCRtpTransceiver](https://developer.mozilla.org/en-US/docs/Web/API/RTCRtpTransceiver) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/RTCRtpTransceiver$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/RTCRtpTransceiver>
**/
@:native("RTCRtpTransceiver")
extern class RtpTransceiver {
	
	/**
		The media ID of the m-line associated with this transceiver. This association is established, when possible, whenever either a local or remote description is applied. This field is `null` if neither a local or remote description has been applied, or if its associated m-line is rejected by either a remote offer or any answer.
	**/
	var mid(default,null) : String;
	
	/**
		The `RTCRtpSender` object responsible for encoding and sending data to the remote peer.
	**/
	var sender(default,null) : RtpSender;
	
	/**
		The `RTCRtpReceiver` object that handles receiving and decoding incoming media.
	**/
	var receiver(default,null) : RtpReceiver;
	
	/**
		Indicates whether or not sending and receiving using the paired `RTCRtpSender` and `RTCRtpReceiver` has been permanently disabled, either due to SDP offer/answer, or due to a call to `RTCRtpTransceiver.stop`.
	**/
	var stopped(default,null) : Bool;
	
	/**
		A string from the enum `RTCRtpTransceiverDirection` which is used to set the transceiver's desired direction.
	**/
	var direction : RtpTransceiverDirection;
	
	/**
		A string from the enum `RTCRtpTransceiverDirection` which indicates the transceiver's current directionality, or `null` if the transceiver is stopped or has never participated in an exchange of offers and answers.
	**/
	var currentDirection(default,null) : RtpTransceiverDirection;
	
	function stop() : Void;
	function getRemoteTrackId() : String;
}