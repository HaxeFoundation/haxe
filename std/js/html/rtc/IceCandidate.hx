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

// This file is generated from mozilla\RTCIceCandidate.webidl. Do not edit!

package js.html.rtc;

/**
	The `RTCIceCandidate` interface—part of the WebRTC API—represents a candidate Internet Connectivity Establishment (ICE) configuration which may be used to establish an `RTCPeerConnection`.

	Documentation [RTCIceCandidate](https://developer.mozilla.org/en-US/docs/Web/API/RTCIceCandidate) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/RTCIceCandidate$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/RTCIceCandidate>
**/
@:native("RTCIceCandidate")
extern class IceCandidate {
	
	/**
		A `DOMString` representing the transport address for the candidate that can be used for connectivity checks. The format of this address is a `candidate-attribute` as defined in {{RFC(5245)}}. This string is empty (`""`) if the `RTCIceCandidate` is an "end of candidates" indicator.
	**/
	var candidate : String;
	
	/**
		A `DOMString` specifying the candidate's media stream identification tag which uniquely identifies the media stream within the component with which the candidate is associated, or `null` if no such association exists.
	**/
	var sdpMid : String;
	
	/**
		If not `null`, `sdpMLineIndex` indicates the zero-based index number of the media description (as defined in RFC 4566) in the `SDP` with which the candidate is associated.
	**/
	var sdpMLineIndex : Int;
	
	/** @throws DOMError */
	function new( candidateInitDict : IceCandidateInit ) : Void;
	function toJSON() : Dynamic;
}