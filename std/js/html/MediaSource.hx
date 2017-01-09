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

// This file is generated from mozilla\MediaSource.webidl. Do not edit!

package js.html;

/**
	The `MediaSource` interface represents a source of media data for an `HTMLMediaElement` object. A `MediaSource` object can be attached to a `HTMLMediaElement` to be played in the user agent.

	Documentation [MediaSource](https://developer.mozilla.org/en-US/docs/Web/API/MediaSource) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/MediaSource$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/MediaSource>
**/
@:native("MediaSource")
extern class MediaSource extends EventTarget
{
	static 
	/**
		Returns a `Boolean` value indicating if the given MIME type is supported by the current user agent — this is, if it can successfully create `SourceBuffer` objects for that MIME type.
	**/
	function isTypeSupported( type : String ) : Bool;
	
	/**
		Returns a `SourceBufferList` object containing the list of `SourceBuffer` objects associated with this `MediaSource`.
	**/
	var sourceBuffers(default,null) : SourceBufferList;
	
	/**
		Returns a `SourceBufferList` object containing a subset of the `SourceBuffer` objects contained within `SourceBuffers` — the list of objects providing the selected video track,  enabled audio tracks, and shown/hidden text tracks.
	**/
	var activeSourceBuffers(default,null) : SourceBufferList;
	
	/**
		Returns an enum representing the state of the current `MediaSource`, whether it is not currently attached to a media element (`closed`), attached and ready to receive `SourceBuffer` objects (`open`), or attached but the stream has been ended via `MediaSource.endOfStream()` (`ended`.)
	**/
	var readyState(default,null) : MediaSourceReadyState;
	
	/**
		Gets and sets the duration of the current media being presented.
	**/
	var duration : Float;
	
	/** @throws DOMError */
	function new() : Void;
	/** @throws DOMError */
	
	/**
		Creates a new `SourceBuffer` of the given MIME type and adds it to the `MediaSource`'s `SourceBuffers` list.
	**/
	function addSourceBuffer( type : String ) : SourceBuffer;
	/** @throws DOMError */
	
	/**
		Removes the given `SourceBuffer` from the `SourceBuffers` list associated with this `MediaSource` object.
	**/
	function removeSourceBuffer( sourceBuffer : SourceBuffer ) : Void;
	/** @throws DOMError */
	
	/**
		Signals the end of the stream.
	**/
	function endOfStream( ?error : MediaSourceEndOfStreamError ) : Void;
}