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

// This file is generated from mozilla\RTCPeerConnection.webidl. Do not edit!

package js.html.rtc;

/**
	The `RTCPeerConnection` interface represents a WebRTC connection between the local computer and a remote peer. It provides methods to connect to a remote peer, maintain and monitor the connection, and close the connection once it's no longer needed.

	Documentation [RTCPeerConnection](https://developer.mozilla.org/en-US/docs/Web/API/RTCPeerConnection) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/RTCPeerConnection$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/RTCPeerConnection>
**/
@:native("RTCPeerConnection")
extern class PeerConnection extends js.html.EventTarget
{
	/** @throws DOMError */
	static function generateCertificate( keygenAlgorithm : haxe.extern.EitherType<Dynamic,String> ) : Promise<Certificate>;
	var localDescription(default,null) : SessionDescription;
	var remoteDescription(default,null) : SessionDescription;
	var signalingState(default,null) : SignalingState;
	var canTrickleIceCandidates(default,null) : Bool;
	var iceGatheringState(default,null) : IceGatheringState;
	var iceConnectionState(default,null) : IceConnectionState;
	var peerIdentity(default,null) : Promise<IdentityAssertion>;
	var idpLoginUrl(default,null) : String;
	var onnegotiationneeded : haxe.Constraints.Function;
	var onicecandidate : haxe.Constraints.Function;
	var onsignalingstatechange : haxe.Constraints.Function;
	var onaddstream : haxe.Constraints.Function;
	var onaddtrack : haxe.Constraints.Function;
	var ontrack : haxe.Constraints.Function;
	var onremovestream : haxe.Constraints.Function;
	var oniceconnectionstatechange : haxe.Constraints.Function;
	var ondatachannel : haxe.Constraints.Function;
	
	/** @throws DOMError */
	function new( ?configuration : Configuration, ?constraints : Dynamic ) : Void;
	function setIdentityProvider( provider : String, ?protocol : String, ?username : String ) : Void;
	function getIdentityAssertion() : Promise<String>;
	@:overload( function( ?options : OfferOptions ) : Promise<SessionDescription> {} )
	function createOffer( successCallback : SessionDescription -> Void, failureCallback : js.html.DOMError -> Void, ?options : OfferOptions ) : Promise<Void>;
	@:overload( function( ?options : AnswerOptions ) : Promise<SessionDescription> {} )
	function createAnswer( successCallback : SessionDescription -> Void, failureCallback : js.html.DOMError -> Void ) : Promise<Void>;
	@:overload( function( description : SessionDescription ) : Promise<Void> {} )
	function setLocalDescription( description : SessionDescription, successCallback : Void -> Void, failureCallback : js.html.DOMError -> Void ) : Promise<Void>;
	@:overload( function( description : SessionDescription ) : Promise<Void> {} )
	function setRemoteDescription( description : SessionDescription, successCallback : Void -> Void, failureCallback : js.html.DOMError -> Void ) : Promise<Void>;
	@:overload( function( candidate : IceCandidate ) : Promise<Void> {} )
	function addIceCandidate( candidate : IceCandidate, successCallback : Void -> Void, failureCallback : js.html.DOMError -> Void ) : Promise<Void>;
	function getConfiguration() : Configuration;
	function getLocalStreams() : Array<js.html.MediaStream>;
	function getRemoteStreams() : Array<js.html.MediaStream>;
	function getStreamById( streamId : String ) : js.html.MediaStream;
	function addStream( stream : js.html.MediaStream ) : Void;
	function removeStream( stream : js.html.MediaStream ) : Void;
	function addTrack( track : js.html.MediaStreamTrack, stream : js.html.MediaStream, moreStreams : haxe.extern.Rest<js.html.MediaStream> ) : RtpSender;
	function removeTrack( sender : RtpSender ) : Void;
	function getSenders() : Array<RtpSender>;
	function getReceivers() : Array<RtpReceiver>;
	function close() : Void;
	@:overload( function( ?selector : js.html.MediaStreamTrack ) : Promise<StatsReport> {} )
	function getStats( selector : js.html.MediaStreamTrack, successCallback : StatsReport -> Void, failureCallback : js.html.DOMError -> Void ) : Promise<Void>;
	function createDataChannel( label : String, ?dataChannelDict : DataChannelInit ) : Dynamic/*MISSING RTCDataChannel*/;
}