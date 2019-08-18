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

// This file is generated from mozilla\RTCDataChannelEvent.webidl. Do not edit!

package js.html.rtc;

/**
	The `RTCDataChannelEvent()` constructor returns a new `RTCDataChannelEvent` object, which represents a `datachannel` event. These events sent to an `RTCPeerConnection` when its remote peer is asking to open an `RTCDataChannel` between the two peers.

	Documentation [RTCDataChannelEvent](https://developer.mozilla.org/en-US/docs/Web/API/RTCDataChannelEvent) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/RTCDataChannelEvent$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/RTCDataChannelEvent>
**/
@:native("RTCDataChannelEvent")
extern class DataChannelEvent extends js.html.Event {
	var channel(default,null) : DataChannel;
	
	/** @throws DOMError */
	function new( type : String, eventInitDict : DataChannelEventInit ) : Void;
}