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

// This file is generated from mozilla/MediaRecorder.webidl line 18:0. Do not edit!

package js.html;

@:native("MediaRecorder")
extern class MediaRecorder extends EventTarget
{
	var stream(default,null) : MediaStream;
	var state(default,null) : RecordingState;
	var mimeType(default,null) : String;
	var ondataavailable : haxe.Constraints.Function;
	var onerror : haxe.Constraints.Function;
	var onstart : haxe.Constraints.Function;
	var onstop : haxe.Constraints.Function;
	var onwarning : haxe.Constraints.Function;
	
	/** @throws DOMError */
	@:overload( function( stream : MediaStream, ?options : MediaRecorderOptions ) : Void {} )
	function new( node : js.html.audio.AudioNode, ?output : Int = 0, ?options : MediaRecorderOptions ) : Void;
	/** @throws DOMError */
	function start( ?timeSlice : Int ) : Void;
	/** @throws DOMError */
	function stop() : Void;
	/** @throws DOMError */
	function pause() : Void;
	/** @throws DOMError */
	function resume() : Void;
	/** @throws DOMError */
	function requestData() : Void;
}