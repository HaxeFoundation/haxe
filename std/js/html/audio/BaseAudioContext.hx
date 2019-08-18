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

// This file is generated from mozilla\BaseAudioContext.webidl. Do not edit!

package js.html.audio;

import js.lib.Promise;

/**
	The `BaseAudioContext` interface acts as a base definition for online and offline audio-processing graphs, as represented by `AudioContext` and `OfflineAudioContext` respectively.

	Documentation [BaseAudioContext](https://developer.mozilla.org/en-US/docs/Web/API/BaseAudioContext) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/BaseAudioContext$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/BaseAudioContext>
**/
@:native("BaseAudioContext")
extern class BaseAudioContext extends js.html.EventTarget {

	/**
		Returns an `AudioDestinationNode` representing the final destination of all audio in the context. It can be thought of as the audio-rendering device.
	**/
	var destination(default,null) : AudioDestinationNode;

	/**
		Returns a float representing the sample rate (in samples per second) used by all nodes in this context. The sample-rate of an `AudioContext` cannot be changed.
	**/
	var sampleRate(default,null) : Float;

	/**
		Returns a double representing an ever-increasing hardware time in seconds used for scheduling. It starts at `0`.
	**/
	var currentTime(default,null) : Float;

	/**
		Returns the `AudioListener` object, used for 3D spatialization.
	**/
	var listener(default,null) : AudioListener;

	/**
		Returns the current state of the `AudioContext`.
	**/
	var state(default,null) : AudioContextState;

	/**
		An event handler that runs when an event of type `statechange` has fired. This occurs when the `AudioContext`'s state changes, due to the calling of one of the state change methods (`AudioContext.suspend`, `AudioContext.resume`, or `AudioContext.close`).
	**/
	var onstatechange : haxe.Constraints.Function;


	/**
		Resumes the progression of time in an audio context that has previously been suspended/paused.
		@throws DOMError
	**/
	function resume() : Promise<Void>;

	/**
		Creates a new, empty `AudioBuffer` object, which can then be populated by data and played via an `AudioBufferSourceNode`.
		@throws DOMError
	**/
	function createBuffer( numberOfChannels : Int, length : Int, sampleRate : Float ) : AudioBuffer;

	/**
		Asynchronously decodes audio file data contained in an `ArrayBuffer`. In this case, the ArrayBuffer is usually loaded from an `XMLHttpRequest`'s `response` attribute after setting the `responseType` to `arraybuffer`. This method only works on complete files, not fragments of audio files.
		@throws DOMError
	**/
	@:overload( function( audioData : js.lib.ArrayBuffer, ?successCallback : AudioBuffer -> Void, ?errorCallback : Void -> Void ) : Promise<AudioBuffer> {} )
	function decodeAudioData( audioData : js.lib.ArrayBuffer, ?successCallback : AudioBuffer -> Void, ?errorCallback : js.html.DOMException -> Void ) : Promise<AudioBuffer>;

	/**
		Creates an `AudioBufferSourceNode`, which can be used to play and manipulate audio data contained within an `AudioBuffer` object. `AudioBuffer`s are created using `AudioContext.createBuffer` or returned by `AudioContext.decodeAudioData` when it successfully decodes an audio track.
		@throws DOMError
	**/
	function createBufferSource() : AudioBufferSourceNode;

	/**
		Creates a `ConstantSourceNode` object, which is an audio source that continuously outputs a monaural (one-channel) sound signal whose samples all have the same value.
		@throws DOMError
	**/
	function createConstantSource() : ConstantSourceNode;

	/**
		Creates a `ScriptProcessorNode`, which can be used for direct audio processing via JavaScript.
		@throws DOMError
	**/
	function createScriptProcessor( bufferSize : Int = 0, numberOfInputChannels : Int = 2, numberOfOutputChannels : Int = 2 ) : ScriptProcessorNode;

	/**
		Creates an `AnalyserNode`, which can be used to expose audio time and frequency data and for example to create data visualisations.
		@throws DOMError
	**/
	function createAnalyser() : AnalyserNode;

	/**
		Creates a `GainNode`, which can be used to control the overall volume of the audio graph.
		@throws DOMError
	**/
	function createGain() : GainNode;

	/**
		Creates a `DelayNode`, which is used to delay the incoming audio signal by a certain amount. This node is also useful to create feedback loops in a Web Audio API graph.
		@throws DOMError
	**/
	function createDelay( maxDelayTime : Float = 1.0 ) : DelayNode;

	/**
		Creates a `BiquadFilterNode`, which represents a second order filter configurable as several different common filter types: high-pass, low-pass, band-pass, etc
		@throws DOMError
	**/
	function createBiquadFilter() : BiquadFilterNode;

	/**
		Creates an `IIRFilterNode`, which represents a second order filter configurable as several different common filter types.
		@throws DOMError
	**/
	function createIIRFilter( feedforward : Array<Float>, feedback : Array<Float> ) : IIRFilterNode;

	/**
		Creates a `WaveShaperNode`, which is used to implement non-linear distortion effects.
		@throws DOMError
	**/
	function createWaveShaper() : WaveShaperNode;

	/**
		Creates a `PannerNode`, which is used to spatialise an incoming audio stream in 3D space.
		@throws DOMError
	**/
	function createPanner() : PannerNode;

	/**
		Creates a `StereoPannerNode`, which can be used to apply stereo panning to an audio source.
		@throws DOMError
	**/
	function createStereoPanner() : StereoPannerNode;

	/**
		Creates a `ConvolverNode`, which can be used to apply convolution effects to your audio graph, for example a reverberation effect.
		@throws DOMError
	**/
	function createConvolver() : ConvolverNode;

	/**
		Creates a `ChannelSplitterNode`, which is used to access the individual channels of an audio stream and process them separately.
		@throws DOMError
	**/
	function createChannelSplitter( numberOfOutputs : Int = 6 ) : ChannelSplitterNode;

	/**
		Creates a `ChannelMergerNode`, which is used to combine channels from multiple audio streams into a single audio stream.
		@throws DOMError
	**/
	function createChannelMerger( numberOfInputs : Int = 6 ) : ChannelMergerNode;

	/**
		Creates a `DynamicsCompressorNode`, which can be used to apply acoustic compression to an audio signal.
		@throws DOMError
	**/
	function createDynamicsCompressor() : DynamicsCompressorNode;

	/**
		Creates an `OscillatorNode`, a source representing a periodic waveform. It basically generates a tone.
		@throws DOMError
	**/
	function createOscillator() : OscillatorNode;

	/**
		Creates a `PeriodicWave`, used to define a periodic waveform that can be used to determine the output of an `OscillatorNode`.
		@throws DOMError
	**/
	function createPeriodicWave( real : js.lib.Float32Array, imag : js.lib.Float32Array, ?constraints : PeriodicWaveConstraints ) : PeriodicWave;
}