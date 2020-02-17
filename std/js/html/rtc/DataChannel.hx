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

// This file is generated from mozilla\RTCDataChannel.webidl. Do not edit!

package js.html.rtc;

/**
	The `RTCDataChannel` interface represents a network channel which can be used for bidirectional peer-to-peer transfers of arbitrary data. Every data channel is associated with an `RTCPeerConnection`, and each peer connection can have up to a theoretical maximum of 65,534 data channels (the actual limit may vary from browser to browser).

	Documentation [RTCDataChannel](https://developer.mozilla.org/en-US/docs/Web/API/RTCDataChannel) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/RTCDataChannel$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/RTCDataChannel>
**/
@:native("RTCDataChannel")
extern class DataChannel extends js.html.EventTarget {
	var label(default,null) : String;
	var reliable(default,null) : Bool;
	var maxPacketLifeTime(default,null) : Int;
	var maxRetransmits(default,null) : Int;
	var readyState(default,null) : DataChannelState;
	var bufferedAmount(default,null) : Int;
	var bufferedAmountLowThreshold : Int;
	var onopen : haxe.Constraints.Function;
	var onerror : haxe.Constraints.Function;
	var onclose : haxe.Constraints.Function;
	var onmessage : haxe.Constraints.Function;
	var onbufferedamountlow : haxe.Constraints.Function;
	var binaryType : DataChannelType;
	var protocol(default,null) : String;
	var ordered(default,null) : Bool;
	var id(default,null) : Int;
	
	function close() : Void;
	/** @throws DOMError */
	@:overload( function( data : String ) : Void {} )
	@:overload( function( data : js.html.Blob ) : Void {} )
	@:overload( function( data : js.lib.ArrayBuffer ) : Void {} )
	function send( data : js.lib.ArrayBufferView ) : Void;
}