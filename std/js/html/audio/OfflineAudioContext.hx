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

// This file is generated from mozilla\OfflineAudioContext.webidl. Do not edit!

package js.html.audio;

/**
	The `OfflineAudioContext` interface is an `AudioContext` interface representing an audio-processing graph built from linked together `AudioNode`s. In contrast with a standard `AudioContext`, an `OfflineAudioContext` doesn't render the audio to the device hardware; instead, it generates it, as fast as it can, and outputs the result to an `AudioBuffer`.

	Documentation [OfflineAudioContext](https://developer.mozilla.org/en-US/docs/Web/API/OfflineAudioContext) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/OfflineAudioContext$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/OfflineAudioContext>
**/
@:native("OfflineAudioContext")
extern class OfflineAudioContext extends AudioContext
{
	
	/**
		Is an `EventHandler` called when processing is terminated, that is when the `complete` event (of type `OfflineAudioCompletionEvent`) is raised, after the event-based version of `OfflineAudioContext.startRendering()` is used.
	**/
	var oncomplete : haxe.Constraints.Function;
	
	/** @throws DOMError */
	function new( numberOfChannels : Int, length : Int, sampleRate : Float ) : Void;
	/** @throws DOMError */
	
	/**
		Starts rendering the audio, taking into account the current connections and the current scheduled changes. This page covers both the event-based version and the promise-based version.
	**/
	function startRendering() : Promise<AudioBuffer>;
}