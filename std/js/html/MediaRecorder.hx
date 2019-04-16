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

// This file is generated from mozilla\MediaRecorder.webidl. Do not edit!

package js.html;

/**
	The `MediaRecorder` interface of the MediaStream Recording API provides functionality to easily record media. It is created using the `MediaRecorder()` constructor.

	Documentation [MediaRecorder](https://developer.mozilla.org/en-US/docs/Web/API/MediaRecorder) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/MediaRecorder$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/MediaRecorder>
**/
@:native("MediaRecorder")
extern class MediaRecorder extends EventTarget {
	static function isTypeSupported( type : String ) : Bool;
	
	/**
		Returns the stream that was passed into the constructor when the `MediaRecorder` was created.
	**/
	var stream(default,null) : MediaStream;
	
	/**
		Returns the current state of the `MediaRecorder` object (`inactive`, `recording`, or `paused`.)
	**/
	var state(default,null) : RecordingState;
	
	/**
		Returns the MIME type that was selected as the recording container for the `MediaRecorder` object when it was created.
	**/
	var mimeType(default,null) : String;
	var ondataavailable : haxe.Constraints.Function;
	var onerror : haxe.Constraints.Function;
	var onstart : haxe.Constraints.Function;
	var onstop : haxe.Constraints.Function;
	var onwarning : haxe.Constraints.Function;
	
	/** @throws DOMError */
	@:overload( function( stream : MediaStream, ?options : MediaRecorderOptions ) : Void {} )
	function new( node : js.html.audio.AudioNode, output : Int = 0, ?options : MediaRecorderOptions ) : Void;
	
	/**
		Begins recording media; this method can optionally be passed a `timeslice` argument with a value in milliseconds. If this is specified, the media will be captured in separate chunks of that duration, rather than the default behavior of recording the media in a single large chunk.
		@throws DOMError
	**/
	function start( ?timeSlice : Int ) : Void;
	
	/**
		Stops recording, at which point a `dataavailable` event containing the final `Blob` of saved data is fired. No more recording occurs.
		@throws DOMError
	**/
	function stop() : Void;
	
	/**
		Pauses the recording of media.
		@throws DOMError
	**/
	function pause() : Void;
	
	/**
		Resumes recording of media after having been paused.
		@throws DOMError
	**/
	function resume() : Void;
	
	/**
		Requests a `Blob` containing the saved data received thus far (or since the last time `requestData()` was called. After calling this method, recording continues, but in a new `Blob`.
		@throws DOMError
	**/
	function requestData() : Void;
}