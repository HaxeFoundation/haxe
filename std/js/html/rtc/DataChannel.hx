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

// This file is generated from mozilla\DataChannel.webidl. Do not edit!

package js.html.rtc;

@:native("DataChannel")
extern class DataChannel extends js.html.EventTarget
{
	var label(default,null) : String;
	var reliable(default,null) : Bool;
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
	var stream(default,null) : Int;
	
	function close() : Void;
	/** @throws DOMError */
	@:overload( function( data : String ) : Void {} )
	@:overload( function( data : js.html.Blob ) : Void {} )
	@:overload( function( data : js.html.ArrayBuffer ) : Void {} )
	function send( data : js.html.ArrayBufferView ) : Void;
}