/*
 * Copyright (C)2005-2013 Haxe Foundation
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

// This file is generated, do not edit!
package js.html.rtc;

@:native("RTCPeerConnection")
extern class PeerConnection extends js.html.EventTarget
{
	var iceState(default,null) : String;

	/** Getter throws DOMException. */
	var localDescription(default,null) : SessionDescription;

	var localStreams(default,null) : MediaStreamList;

	var onaddstream : js.html.EventListener;

	var ondatachannel : js.html.EventListener;

	var onicecandidate : js.html.EventListener;

	var onicechange : js.html.EventListener;

	var onnegotiationneeded : js.html.EventListener;

	var onopen : js.html.EventListener;

	var onremovestream : js.html.EventListener;

	var onstatechange : js.html.EventListener;

	var readyState(default,null) : String;

	/** Getter throws DOMException. */
	var remoteDescription(default,null) : SessionDescription;

	var remoteStreams(default,null) : MediaStreamList;

	function new() : Void;

	function addIceCandidate( candidate : IceCandidate ) : Void;

	function addStream( stream : MediaStream, ?mediaConstraints : Dynamic ) : Void;

	function close() : Void;

	function createAnswer( successCallback : SessionDescriptionCallback, failureCallback : ErrorCallback, ?mediaConstraints : Dynamic ) : Void;

	function createDataChannel( label : String, ?options : Dynamic ) : DataChannel;

	function createOffer( successCallback : SessionDescriptionCallback, failureCallback : ErrorCallback, ?mediaConstraints : Dynamic ) : Void;

	function getStats( successCallback : StatsCallback, selector : MediaStreamTrack ) : Void;

	function removeStream( stream : MediaStream ) : Void;

	function setLocalDescription( description : SessionDescription, successCallback : js.html.VoidCallback, failureCallback : ErrorCallback ) : Void;

	function setRemoteDescription( description : SessionDescription, successCallback : js.html.VoidCallback, failureCallback : ErrorCallback ) : Void;

	function updateIce( ?configuration : Dynamic, ?mediaConstraints : Dynamic ) : Void;

}
