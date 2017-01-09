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

// This file is generated from mozilla\AudioBufferSourceNode.webidl. Do not edit!

package js.html.audio;

/**
	The `AudioBufferSourceNode` interface represents an audio source consisting of in-memory audio data, stored in an `AudioBuffer`. It is an `AudioNode` that acts as an audio source`.`

	Documentation [AudioBufferSourceNode](https://developer.mozilla.org/en-US/docs/Web/API/AudioBufferSourceNode) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/AudioBufferSourceNode$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/AudioBufferSourceNode>
**/
@:native("AudioBufferSourceNode")
extern class AudioBufferSourceNode extends AudioNode
{
	
	/**
		Is an `AudioBuffer` that defines the audio asset to be played, or when set to the value `null`, defines a single channel of silence. 
	**/
	var buffer : AudioBuffer;
	
	/**
		Is an a-rate `AudioParam` that defines the speed factor at which the audio asset will be played. Since no pitch correction is applied on the output, this can be used to change the pitch of the sample.
	**/
	var playbackRate(default,null) : AudioParam;
	
	/**
		Is a `AudioParam` representing detuning of oscillation in cents. Its default value is `0`.
	**/
	var detune(default,null) : AudioParam;
	
	/**
		Is a Boolean attribute indicating if the audio asset must be replayed when the end of the `AudioBuffer` is reached. Its default value is `false`.
	**/
	var loop : Bool;
	
	/**
		Is a double value indicating, in seconds, where in the `AudioBuffer` the restart of the play must happen. Its default value is `0`.
	**/
	var loopStart : Float;
	
	/**
		Is a double value indicating, in seconds, where in the `AudioBuffer` the replay of the play must stop (and eventually loop again). Its default value is `0`.
	**/
	var loopEnd : Float;
	
	/**
		Is an `EventHandler` containing the callback associated with the `ended_(Web_Audio)` event.
	**/
	var onended : haxe.Constraints.Function;
	
	/** @throws DOMError */
	
	/**
		Schedules the start of the playback of the audio asset.
	**/
	function start( ?when : Float = 0.0, ?grainOffset : Float = 0.0, ?grainDuration : Float ) : Void;
	/** @throws DOMError */
	
	/**
		Schedules the end of the playback of an audio asset.
	**/
	function stop( ?when : Float = 0.0 ) : Void;
}