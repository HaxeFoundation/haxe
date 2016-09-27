/*
 * Copyright (C)2005-2016 Haxe Foundation
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

// This file is generated from mozilla\MediaStream.webidl line 46:0. Do not edit!

package js.html;

@:native("MediaStream")
extern class MediaStream extends EventTarget
{
	var id(default,null) : String;
	var currentTime(default,null) : Float;
	
	/** @throws DOMError */
	@:overload( function() : Void {} )
	@:overload( function( stream : MediaStream ) : Void {} )
	function new( tracks : Array<MediaStreamTrack> ) : Void;
	function getAudioTracks() : Array<AudioStreamTrack>;
	function getVideoTracks() : Array<VideoStreamTrack>;
	function getTracks() : Array<MediaStreamTrack>;
	function addTrack( track : MediaStreamTrack ) : Void;
	function removeTrack( track : MediaStreamTrack ) : Void;
}