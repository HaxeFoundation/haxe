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

@:native("RTCDataChannel")
extern class DataChannel extends js.html.EventTarget
{
	/** Setter throws DOMException. */
	var binaryType : String;

	var bufferedAmount(default,null) : Int;

	var label(default,null) : String;

	var onclose : js.html.EventListener;

	var onerror : js.html.EventListener;

	var onmessage : js.html.EventListener;

	var onopen : js.html.EventListener;

	var readyState(default,null) : String;

	var reliable(default,null) : Bool;

	function close() : Void;

	/** Throws DOMException. */
	@:overload( function( data : js.html.ArrayBuffer ) :Void {} )
	@:overload( function( data : js.html.ArrayBufferView ) :Void {} )
	@:overload( function( data : js.html.Blob ) :Void {} )
	function send( data : String ) : Void;

}
