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

// This file is generated from mozilla\AudioContext.webidl. Do not edit!

package js.html.audio;

import js.lib.Promise;

/**
	The `AudioContext` interface represents an audio-processing graph built from audio modules linked together, each represented by an `AudioNode`.

	Documentation [AudioContext](https://developer.mozilla.org/en-US/docs/Web/API/AudioContext) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/AudioContext$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/AudioContext>
**/
@:native("AudioContext")
extern class AudioContext extends BaseAudioContext {
	/** @throws DOMError */
	function new( ?contextOptions : AudioContextOptions ) : Void;

	/**
		Suspends the progression of time in the audio context, temporarily halting audio hardware access and reducing CPU/battery usage in the process.
		@throws DOMError
	**/
	function suspend() : Promise<Void>;

	/**
		Closes the audio context, releasing any system audio resources that it uses.
		@throws DOMError
	**/
	function close() : Promise<Void>;

	/**
		Creates a `MediaElementAudioSourceNode` associated with an `HTMLMediaElement`. This can be used to play and manipulate audio from `video` or `audio` elements.
		@throws DOMError
	**/
	function createMediaElementSource( mediaElement : js.html.MediaElement ) : MediaElementAudioSourceNode;

	/**
		Creates a `MediaStreamAudioSourceNode` associated with a `MediaStream` representing an audio stream which may come from the local computer microphone or other sources.
		@throws DOMError
	**/
	function createMediaStreamSource( mediaStream : js.html.MediaStream ) : MediaStreamAudioSourceNode;

	/**
		Creates a `MediaStreamAudioDestinationNode` associated with a `MediaStream` representing an audio stream which may be stored in a local file or sent to another computer.
		@throws DOMError
	**/
	function createMediaStreamDestination() : MediaStreamAudioDestinationNode;
}