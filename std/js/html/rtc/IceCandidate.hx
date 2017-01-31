/*
 * Copyright (C)2005-2017 Haxe Foundation
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

// This file is generated from mozilla\RTCIceCandidate.webidl. Do not edit!

package js.html.rtc;

/**
	The `RTCIceCandidate` interface of the the WebRTC API represents a candidate internet connectivity establishment (ICE) server for establishing an `RTCPeerConnection`.

	Documentation [RTCIceCandidate](https://developer.mozilla.org/en-US/docs/Web/API/RTCIceCandidate) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/RTCIceCandidate$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/RTCIceCandidate>
**/
@:native("RTCIceCandidate")
extern class IceCandidate
{
	
	/**
		Returns a transport address for the candidate that can be used for connectivity checks. The format of this address is a `candidate-attribute` as defined in RTC 5245.
	**/
	var candidate : String;
	
	/**
		If not `null`, this contains the identifier of the "media stream identification" (as defined in RFC 5888) for the media component this candidate is associated with.
	**/
	var sdpMid : String;
	
	/**
		If not `null`, this indicates the index (starting at zero) of the media description (as defined in RFC 4566) in the SDP this candidate is associated with.
	**/
	var sdpMLineIndex : Int;
	
	/** @throws DOMError */
	function new( ?candidateInitDict : IceCandidateInit ) : Void;
}