/*
 * Copyright (C)2005-2015 Haxe Foundation
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

// This file is generated from mozilla/MediaSource.webidl line 25:0. Do not edit!

package js.html;

@:native("MediaSource")
extern class MediaSource extends EventTarget
{
	static function isTypeSupported( type : String ) : Bool;
	var sourceBuffers(default,null) : SourceBufferList;
	var activeSourceBuffers(default,null) : SourceBufferList;
	var readyState(default,null) : MediaSourceReadyState;
	var duration : Float;
	
	/** @throws DOMError */
	function new() : Void;
	/** @throws DOMError */
	function addSourceBuffer( type : String ) : SourceBuffer;
	/** @throws DOMError */
	function removeSourceBuffer( sourceBuffer : SourceBuffer ) : Void;
	/** @throws DOMError */
	function endOfStream( ?error : MediaSourceEndOfStreamError ) : Void;
}