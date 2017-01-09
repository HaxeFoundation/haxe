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

// This file is generated from mozilla\MediaStream.webidl. Do not edit!

package js.html;

/**
	The `MediaStream` interface represents a stream of media content. A stream consists of several tracks such as video or audio tracks. Each track is specified as an instance of `MediaStreamTrack`.

	Documentation [MediaStream](https://developer.mozilla.org/en-US/docs/Web/API/MediaStream) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/MediaStream$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/MediaStream>
**/
@:native("MediaStream")
extern class MediaStream extends EventTarget
{
	
	/**
		A `DOMString` containing 36 characters denoting a universally unique identifier (UUID) for the object.
	**/
	var id(default,null) : String;
	var currentTime(default,null) : Float;
	
	/** @throws DOMError */
	@:overload( function() : Void {} )
	@:overload( function( stream : MediaStream ) : Void {} )
	function new( tracks : Array<MediaStreamTrack> ) : Void;
	function getAudioTracks() : Array<AudioStreamTrack>;
	
	/**
		Returns a list of the `MediaStreamTrack` objects stored in the `MediaStream` object that have their `kind` attribute set to `"video"`. The order is not defined, and may not only vary from one browser to another, but also from one call to another.
	**/
	function getVideoTracks() : Array<VideoStreamTrack>;
	
	/**
		Returns a list of all `MediaStreamTrack` objects stored in the `MediaStream` object, regardless of the value of the `kind` attribute. The order is not defined, and may not only vary from one browser to another, but also from one call to another.
	**/
	function getTracks() : Array<MediaStreamTrack>;
	
	/**
		Stores a copy of the `MediaStreamTrack` given as argument. If the track has already been added to the `MediaStream` object, nothing happens.
	**/
	function addTrack( track : MediaStreamTrack ) : Void;
	
	/**
		Removes the `MediaStreamTrack` given as argument. If the track is not part of the MediaStream` object, nothing happens.
	**/
	function removeTrack( track : MediaStreamTrack ) : Void;
}