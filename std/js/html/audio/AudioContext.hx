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

// This file is generated from mozilla\AudioContext.webidl. Do not edit!

package js.html.audio;

/**
	An `AudioContext` can be a target of events, therefore it implements the `EventTarget` interface.

	Documentation [AudioContext](https://developer.mozilla.org/en-US/docs/Web/API/AudioContext) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/AudioContext$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/AudioContext>
**/
@:native("AudioContext")
extern class AudioContext extends js.html.EventTarget
{
	
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
	var state(default,null) : js.html.AudioContextState;
	
	/**
		An event handler that runs when an event of type `statechange` has fired. This occurs when the `AudioContext`'s state changes, due to the calling of one of the state change methods (`AudioContext.suspend`, `AudioContext.resume`, or `AudioContext.close`).
	**/
	var onstatechange : haxe.Constraints.Function;
	
	/** @throws DOMError */
	@:overload( function() : Void {} )
	function new( audioChannelType : js.html.AudioChannel ) : Void;
	/** @throws DOMError */
	
	/**
		Suspends the progression of time in the audio context, temporarily halting audio hardware access and reducing CPU/battery usage in the process.
	**/
	function suspend() : Promise<Void>;
	/** @throws DOMError */
	
	/**
		Resumes the progression of time in an audio context that has previously been suspended.
	**/
	function resume() : Promise<Void>;
	/** @throws DOMError */
	
	/**
		Closes the audio context, releasing any system audio resources that it uses.
	**/
	function close() : Promise<Void>;
	/** @throws DOMError */
	
	/**
		Creates a new, empty `AudioBuffer` object, which can then be populated by data and played via an `AudioBufferSourceNode`.
	**/
	function createBuffer( numberOfChannels : Int, length : Int, sampleRate : Float ) : AudioBuffer;
	/** @throws DOMError */
	
	/**
		Asynchronously decodes audio file data contained in an `ArrayBuffer`. In this case, the ArrayBuffer is usually loaded from an `XMLHttpRequest`'s `response` attribute after setting the `responseType` to `arraybuffer`. This method only works on complete files, not fragments of audio files.
	**/
	function decodeAudioData( audioData : js.html.ArrayBuffer, ?successCallback : AudioBuffer -> Void, ?errorCallback : Void -> Void ) : Promise<AudioBuffer>;
	/** @throws DOMError */
	
	/**
		Creates an `AudioBufferSourceNode`, which can be used to play and manipulate audio data contained within an `AudioBuffer` object. `AudioBuffer`s are created using `AudioContext.createBuffer` or returned by `AudioContext.decodeAudioData` when it successfully decodes an audio track.
	**/
	function createBufferSource() : AudioBufferSourceNode;
	/** @throws DOMError */
	
	/**
		Creates a `MediaStreamAudioDestinationNode` associated with a `MediaStream` representing an audio stream which may be stored in a local file or sent to another computer.
	**/
	function createMediaStreamDestination() : MediaStreamAudioDestinationNode;
	/** @throws DOMError */
	
	/**
		Creates a `ScriptProcessorNode`, which can be used for direct audio processing via JavaScript.
	**/
	function createScriptProcessor( ?bufferSize : Int = 0, ?numberOfInputChannels : Int = 2, ?numberOfOutputChannels : Int = 2 ) : ScriptProcessorNode;
	/** @throws DOMError */
	
	/**
		Creates a `StereoPannerNode`, which can be used to apply stereo panning to an audio source.
	**/
	function createStereoPanner() : StereoPannerNode;
	/** @throws DOMError */
	
	/**
		Creates an `AnalyserNode`, which can be used to expose audio time and frequency data and for example to create data visualisations.
	**/
	function createAnalyser() : AnalyserNode;
	/** @throws DOMError */
	
	/**
		Creates a `MediaElementAudioSourceNode` associated with an `HTMLMediaElement`. This can be used to play and manipulate audio from `video` or `audio` elements.
	**/
	function createMediaElementSource( mediaElement : js.html.MediaElement ) : MediaElementAudioSourceNode;
	/** @throws DOMError */
	
	/**
		Creates a `MediaStreamAudioSourceNode` associated with a `MediaStream` representing an audio stream which may come from the local computer microphone or other sources.
	**/
	function createMediaStreamSource( mediaStream : js.html.MediaStream ) : MediaStreamAudioSourceNode;
	/** @throws DOMError */
	
	/**
		Creates a `GainNode`, which can be used to control the overall volume of the audio graph.
	**/
	function createGain() : GainNode;
	/** @throws DOMError */
	
	/**
		Creates a `DelayNode`, which is used to delay the incoming audio signal by a certain amount. This node is also useful to create feedback loops in a Web Audio API graph.
	**/
	function createDelay( ?maxDelayTime : Float = 1.0 ) : DelayNode;
	/** @throws DOMError */
	
	/**
		Creates a `BiquadFilterNode`, which represents a second order filter configurable as several different common filter types: high-pass, low-pass, band-pass, etc.
	**/
	function createBiquadFilter() : BiquadFilterNode;
	/** @throws DOMError */
	
	/**
		Creates a `WaveShaperNode`, which is used to implement non-linear distortion effects.
	**/
	function createWaveShaper() : WaveShaperNode;
	/** @throws DOMError */
	
	/**
		Creates a `PannerNode`, which is used to spatialise an incoming audio stream in 3D space.
	**/
	function createPanner() : PannerNode;
	/** @throws DOMError */
	
	/**
		Creates a `ConvolverNode`, which can be used to apply convolution effects to your audio graph, for example a reverberation effect.
	**/
	function createConvolver() : ConvolverNode;
	/** @throws DOMError */
	
	/**
		Creates a `ChannelSplitterNode`, which is used to access the individual channels of an audio stream and process them separately.
	**/
	function createChannelSplitter( ?numberOfOutputs : Int = 6 ) : ChannelSplitterNode;
	/** @throws DOMError */
	
	/**
		Creates a `ChannelMergerNode`, which is used to combine channels from multiple audio streams into a single audio stream.
	**/
	function createChannelMerger( ?numberOfInputs : Int = 6 ) : ChannelMergerNode;
	/** @throws DOMError */
	
	/**
		Creates a `DynamicsCompressorNode`, which can be used to apply acoustic compression to an audio signal.
	**/
	function createDynamicsCompressor() : DynamicsCompressorNode;
	/** @throws DOMError */
	
	/**
		Creates an `OscillatorNode`, a source representing a periodic waveform. It basically generates a tone.
	**/
	function createOscillator() : OscillatorNode;
	/** @throws DOMError */
	
	/**
		Creates a `PeriodicWave`, used to define a periodic waveform that can be used to determine the output of an `OscillatorNode`.
	**/
	function createPeriodicWave( real : js.html.Float32Array, imag : js.html.Float32Array ) : PeriodicWave;
}