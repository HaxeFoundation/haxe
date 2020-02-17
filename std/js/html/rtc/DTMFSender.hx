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

// This file is generated from mozilla\RTCDTMFSender.webidl. Do not edit!

package js.html.rtc;

@:native("RTCDTMFSender")
extern class DTMFSender extends js.html.EventTarget {
	
	/**
		An `EventHandler` to handle the `tonechange` event, which is sent each time an outbound tone starts or finishes playing.
	**/
	var ontonechange : haxe.Constraints.Function;
	
	/**
		A `DOMString` which contains the list of DTMF tones currently in the queue to be transmitted (tones which have already been played are no longer included in the string). See `RTCDTMFSender.toneBuffer` for details on the format of the tone buffer.
	**/
	var toneBuffer(default,null) : String;
	
	function insertDTMF( tones : String, duration : Int = 100, interToneGap : Int = 70 ) : Void;
}