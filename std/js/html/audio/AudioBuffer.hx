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

// This file is generated from mozilla\AudioBuffer.webidl. Do not edit!

package js.html.audio;

/**
	Objects of these types are designed to hold small audio snippets, typically less than 45 s. For longer sounds, objects implementing the `MediaElementAudioSourceNode` are more suitable. The buffer contains data in the following format:  non-interleaved IEEE754 32-bit linear PCM with a nominal range between `-1` and `+1`, that is, 32bits floating point buffer, with each samples between -1.0 and 1.0. If the `AudioBuffer` has multiple channels, they are stored in separate buffer.

	Documentation [AudioBuffer](https://developer.mozilla.org/en-US/docs/Web/API/AudioBuffer) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/AudioBuffer$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/AudioBuffer>
**/
@:native("AudioBuffer")
extern class AudioBuffer
{
	
	/**
		Returns a float representing the sample rate, in samples per second, of the PCM data stored in the buffer.
	**/
	var sampleRate(default,null) : Float;
	
	/**
		Returns an integer representing the length, in sample-frames, of the PCM data stored in the buffer.
	**/
	var length(default,null) : Int;
	
	/**
		Returns a double representing the duration, in seconds, of the PCM data stored in the buffer.
	**/
	var duration(default,null) : Float;
	
	/**
		Returns an integer representing the number of discrete audio channels described by the PCM data stored in the buffer.
	**/
	var numberOfChannels(default,null) : Int;
	
	/** @throws DOMError */
	
	/**
		Returns a `Float32Array` containing the PCM data associated with the channel, defined by the `channel` parameter (with `0` representing the first channel).
	**/
	function getChannelData( channel : Int ) : js.html.Float32Array;
	/** @throws DOMError */
	
	/**
		Copies the samples from the specified channel of the `AudioBuffer` to the `destination` array.
	**/
	function copyFromChannel( destination : js.html.Float32Array, channelNumber : Int, ?startInChannel : Int = 0 ) : Void;
	/** @throws DOMError */
	
	/**
		Copies the samples to the specified channel of the `AudioBuffer`, from the `source` array.
	**/
	function copyToChannel( source : js.html.Float32Array, channelNumber : Int, ?startInChannel : Int = 0 ) : Void;
}