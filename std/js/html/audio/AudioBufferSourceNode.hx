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

// This file is generated from mozilla\AudioBufferSourceNode.webidl. Do not edit!

package js.html.audio;

/**
	The `AudioBufferSourceNode` interface is an `AudioScheduledSourceNode` which represents an audio source consisting of in-memory audio data, stored in an `AudioBuffer`. It's especially useful for playing back audio which has particularly stringent timing accuracy requirements, such as for sounds that must match a specific rhythm and can be kept in memory rather than being played from disk or the network.

	Documentation [AudioBufferSourceNode](https://developer.mozilla.org/en-US/docs/Web/API/AudioBufferSourceNode) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/AudioBufferSourceNode$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/AudioBufferSourceNode>
**/
@:native("AudioBufferSourceNode")
extern class AudioBufferSourceNode extends AudioScheduledSourceNode {
	
	/**
		An `AudioBuffer` that defines the audio asset to be played, or when set to the value `null`, defines a single channel of silence (in which every sample is 0.0).
	**/
	var buffer : AudioBuffer;
	
	/**
		An a-rate `AudioParam` that defines the speed factor at which the audio asset will be played, where a value of 1.0 is the sound's natural sampling rate. Since no pitch correction is applied on the output, this can be used to change the pitch of the sample. This value is compounded with `detune` to determine the final playback rate.
	**/
	var playbackRate(default,null) : AudioParam;
	
	/**
		Is a k-rate `AudioParam` representing detuning of playback in cents. This value is compounded with `playbackRate` to determine the speed at which the sound is played. Its default value is `0` (meaning no detuning), and its nominal range is -∞ to ∞.
	**/
	var detune(default,null) : AudioParam;
	
	/**
		A Boolean attribute indicating if the audio asset must be replayed when the end of the `AudioBuffer` is reached. Its default value is `false`.
	**/
	var loop : Bool;
	
	/**
		A floating-point value indicating the time, in seconds, at which playback of the `AudioBuffer` must begin when `loop` is `true`. Its default value is `0` (meaning that at the beginning of each loop, playback begins at the start of the audio buffer).
	**/
	var loopStart : Float;
	
	/**
		A floating-point number indicating the time, in seconds, at which playback of the `AudioBuffer` stops and loops back to the time indicated by `loopStart`, if `loop` is `true`. The default value is `0`.
	**/
	var loopEnd : Float;
	
	/** @throws DOMError */
	function new( context : BaseAudioContext, ?options : AudioBufferSourceOptions ) : Void;
	
	/**
		Used to schedule playback of the audio data contained in the buffer, or to begin playback immediately.
		@throws DOMError
	**/
	function start( when : Float = 0.0, grainOffset : Float = 0.0, ?grainDuration : Float ) : Void;
}