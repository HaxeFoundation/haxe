/*
 * Copyright (C)2005-2012 Haxe Foundation
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
package js.html;

@:native("RTCPeerConnection")
extern class RTCPeerConnection extends EventTarget
{
    var iceState (default,null) :String;

    /** Getter throws DOMException. */
    var localDescription (default,null) :RTCSessionDescription;

    var localStreams (default,null) :MediaStreamList;

    var onaddstream :EventListener;

    var ondatachannel :EventListener;

    var onicecandidate :EventListener;

    var onicechange :EventListener;

    var onnegotiationneeded :EventListener;

    var onopen :EventListener;

    var onremovestream :EventListener;

    var onstatechange :EventListener;

    var readyState (default,null) :String;

    /** Getter throws DOMException. */
    var remoteDescription (default,null) :RTCSessionDescription;

    var remoteStreams (default,null) :MediaStreamList;

    function new () :Void;

    function addIceCandidate (candidate :RTCIceCandidate) :Void;

    function addStream (stream :MediaStream, ?mediaConstraints :Dynamic) :Void;

    function close () :Void;

    function createAnswer (successCallback :RTCSessionDescriptionCallback, failureCallback :RTCErrorCallback, ?mediaConstraints :Dynamic) :Void;

    function createDataChannel (label :String, ?options :Dynamic) :RTCDataChannel;

    function createOffer (successCallback :RTCSessionDescriptionCallback, failureCallback :RTCErrorCallback, ?mediaConstraints :Dynamic) :Void;

    function getStats (successCallback :RTCStatsCallback, selector :MediaStreamTrack) :Void;

    function removeStream (stream :MediaStream) :Void;

    function setLocalDescription (description :RTCSessionDescription, successCallback :VoidCallback, failureCallback :RTCErrorCallback) :Void;

    function setRemoteDescription (description :RTCSessionDescription, successCallback :VoidCallback, failureCallback :RTCErrorCallback) :Void;

    function updateIce (?configuration :Dynamic, ?mediaConstraints :Dynamic) :Void;

}
